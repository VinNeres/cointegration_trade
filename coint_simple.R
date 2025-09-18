#' Executa um backtest completo de uma estratégia de pairs trading.
#'
#' @param ticker_y String. O ticker do ativo dependente (Y).
#' @param ticker_x String. O ticker do ativo independente (X).
#' @param from_date Data. Data de início do período de análise.
#' @param to_date Data. Data de fim do período de análise.
#' @param z_window Inteiro. Janela móvel para cálculo do Z-score.
#' @param entry_z Numérico. Threshold de Z-score para entrar na operação.
#' @param exit_z Numérico. Threshold de Z-score para sair da operação.
#' @param stop_z Numérico. Threshold de Z-score para stop-loss.
#' @param max_hold_days Inteiro. Número máximo de dias para manter a posição.
#' @param tc_bps Numérico. Custos de transação em basis points (0.01%).
#' @return Uma lista contendo o data frame com os resultados do backtest e os gráficos.

pairs_backtest <- function(
    ticker_y,
    ticker_x,
    from_date = NULL,
    to_date = NULL,
    z_window = 60,
    entry_z = 2.0,
    exit_z = 0.5,
    stop_z = 3.5,
    max_hold_days = 20,
    tc_bps = 0.0
) {
  # --- Pacotes ---
  require(tidyquant)
  require(tidyverse)
  require(urca)
  require(zoo)
  
  # --- Validação dos Inputs ---
  
  # 1. Verifica se as datas não são nulas ou vazias
  if (is.null(from_date) || is.null(to_date)) {
    stop(
      "Erro: 'from_date' e 'to_date' não podem ser nulos. Por favor, forneça datas válidas."
    )
  }
  
  # 2. Tenta converter as datas para o formato Date para garantir que são válidas
  #    O `tryCatch` lida com o caso em que o input não é um formato de data reconhecível.
  from_date_valid <- try(as.Date(from_date), silent = TRUE)
  to_date_valid <- try(as.Date(to_date), silent = TRUE)
  
  if (inherits(from_date_valid, "try-error") || is.na(from_date_valid)) {
    stop(
      "Erro: 'from_date' inválido. Use um formato reconhecível como 'AAAA-MM-DD'."
    )
  }
  
  if (inherits(to_date_valid, "try-error") || is.na(to_date_valid)) {
    stop(
      "Erro: 'to_date' inválido. Use um formato reconhecível como 'AAAA-MM-DD'."
    )
  }
  
  # 3. Verifica se a data de início não é posterior à data de fim
  if (from_date_valid > to_date_valid) {
    stop("Erro: 'from_date' não pode ser posterior a 'to_date'.")
  }
  
  # --- 1. Aquisição de Dados ---
  message(paste("Baixando dados para", ticker_y, "e", ticker_x))
  message()
  
  prices_wide <- tq_get(
    x = c(ticker_y, ticker_x),
    get = "stock.prices",
    from = from_date,
    to = to_date
  ) %>%
    select(symbol, date, adjusted) %>%
    mutate(symbol = make.names(stringr::str_remove_all(symbol, "[\\^=]"))) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    na.omit()
  
  # --- 2. Validação Estatística ---
  message("Validando o par com regressão e teste de cointegração...")
  ticker_y_adj <- make.names(stringr::str_remove_all(ticker_y, "[\\^=]"))
  ticker_x_adj <- make.names(stringr::str_remove_all(ticker_x, "[\\^=]"))

  regression_formula <- as.formula(paste(ticker_y_adj, "~", ticker_x_adj))
  ratio_model <- lm(regression_formula, data = prices_wide)
  beta <- coef(ratio_model)[2]
  
  prices_with_spread <- prices_wide %>%
    mutate(spread = residuals(ratio_model))
  
  adf_result <- tseries::adf.test(prices_with_spread$spread)
  
  if (adf_result$p.value >= 0.05) {
    cli::cli_alert_info(
      "O par pode não ser cointegrado. P-valor do teste ADF: {round(adf_result$p.value, 3)}"
    )
  }
  
  # --- 3. Geração de Sinais e Posições (Lógica Corrigida) ---
  message("Gerando sinais de negociação...")
  
  signals <- prices_with_spread %>%
    mutate(
      z_score = (spread -
                   zoo::rollmean(spread, z_window, fill = NA, align = "right")) /
        zoo::rollapply(spread, z_window, sd, fill = NA, align = "right")
    ) %>%
    mutate(
      entry_signal = case_when(
        z_score > entry_z & lag(z_score, default = 0) <= entry_z ~ -1,
        z_score < -entry_z & lag(z_score, default = 0) >= -entry_z ~ 1,
        TRUE ~ 0
      )
    )
  
  # Lógica para transformar sinais de entrada em posições mantidas
  positions_logic <- signals %>%
    mutate(trade_id = cumsum(entry_signal != 0)) %>%
    group_by(trade_id) %>%
    mutate(days_in_pos = if_else(trade_id == 0, 0, row_number())) %>%
    # Condições de saída - CORRIGIDO com `&` e `|`
    mutate(
      initial_pos = first(entry_signal),
      exit_long = (initial_pos == 1 & z_score >= -exit_z),
      exit_short = (initial_pos == -1 & z_score <= exit_z),
      stop_loss = !is.na(stop_z) & abs(z_score) >= stop_z,
      max_hold = !is.na(max_hold_days) & days_in_pos >= max_hold_days,
      exit_signal = exit_long | exit_short | stop_loss | max_hold
    ) %>%
    ungroup()
  
  final_positions <- positions_logic %>%
    group_by(trade_id) %>%
    # Encontra o primeiro dia de saída dentro de cada trade
    mutate(exit_day_index = which(exit_signal)[1]) %>%
    # A posição é a inicial do trade, mas se torna 0 a partir do dia de saída
    mutate(
      position = if_else(
        trade_id == 0,
        0,
        if_else(
          !is.na(exit_day_index) & row_number() >= exit_day_index,
          0,
          first(entry_signal)
        )
      )
    ) %>%
    ungroup() %>%
    select(-(trade_id:exit_day_index)) # Limpa colunas auxiliares
  
  # --- 4. Cálculo de Retornos e Curva de Capital ---
  message("Calculando retornos e curva de capital...")
  
  backtest_results <- final_positions %>%
    mutate(
      ret_y = TTR::ROC(!!sym(ticker_y_adj), type = "discrete"),
      ret_x = TTR::ROC(!!sym(ticker_x_adj), type = "discrete")
    ) %>%
    mutate(spread_ret = ret_y - beta * ret_x) %>%
    mutate(position = lag(position, default = 0)) %>%
    mutate(
      gross_ret = position * spread_ret,
      turnover = abs(position - lag(position, default = 0)),
      costs = turnover * (1 + abs(beta)) * (tc_bps / 10000),
      net_ret = gross_ret - costs
    ) %>%
    filter(!is.na(net_ret)) %>%
    mutate(equity_curve = cumprod(1 + net_ret))
  
  # --- 5. Visualização e Retorno ---
  message("Gerando gráficos...")
  
  p_equity <- ggplot(backtest_results, aes(x = date, y = equity_curve)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    labs(
      title = "Curva de Capital da Estratégia de Pairs Trading",
      subtitle = paste(ticker_y, "vs", ticker_x),
      x = "Data",
      y = "Patrimônio"
    ) +
    theme_tq()
  
  p_zscore_pos <- ggplot(backtest_results, aes(x = date)) +
    geom_line(aes(y = z_score), color = "gray50") +
    geom_hline(
      yintercept = c(entry_z, -entry_z),
      color = "red",
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = c(exit_z, -exit_z),
      color = "blue",
      linetype = "dotted"
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    geom_line(aes(y = position * entry_z), color = "purple", linewidth = 1) +
    labs(
      title = "Z-score e Posição na Estratégia",
      subtitle = "Linha Roxa = Posição (Escalonada para visualização)",
      x = "Data",
      y = "Z-score"
    ) +
    theme_tq()
  
  return(
    list(
      results_df = backtest_results,
      plot_equity = p_equity,
      plot_zscore_pos = p_zscore_pos,
      model = ratio_model,
      adf_result = adf_result
    )
  )
}


backtest <- pairs_backtest(
  ticker_y = "^BVSP",
  ticker_x = "^GSPC",
  from_date = as.Date("2000-01-01"),
  to_date = as.Date("2010-01-01")
)

print(backtest$plot_equity)
print(backtest$plot_zscore_pos)



#' @title Realiza um Backtest de Pairs Trading com Análise Walk-Forward (Out-of-Sample)
#'
#' @description Esta função simula uma estratégia de pairs trading usando uma abordagem
#' out-of-sample (walk-forward) para fornecer uma avaliação mais realista do desempenho.
#' O modelo é treinado em um período "in-sample" e depois aplicado a um período
#' subsequente "out-of-sample", com as janelas deslizando ao longo do tempo.
#'
#' @param in_sample_days Número de dias de negociação para o período de treino (in-sample).
#' @param out_of_sample_days Número de dias de negociação para o período de teste (out-of-sample), onde os resultados são registrados.
#' @return Uma lista contendo o dataframe com os resultados consolidados (out-of-sample)
#' e os gráficos de curva de capital e z-score/posição.

pairs_walkforward <- function(
    ticker_y,
    ticker_x,
    from_date,
    to_date,
    in_sample_days = 252, # Padrão: ~1 ano de treino
    out_of_sample_days = 63, # Padrão: ~1 trimestre de teste
    entry_z = 2.0,
    exit_z = 0.5,
    stop_z = 3.5,
    max_hold_days = 20,
    tc_bps = 0.0
) {
  # --- Pacotes ---
  require(tidyquant)
  require(tidyverse)
  require(cli)
  
  # --- 1. Aquisição de Todos os Dados de Uma Vez ---
  message(paste("Baixando dados para o período completo:", from_date, "a", to_date))
  
  prices_wide_full <- tq_get(
    x = c(ticker_y, ticker_x),
    get = "stock.prices",
    from = from_date,
    to = to_date
  ) %>%
    select(symbol, date, adjusted) %>%
    mutate(symbol = make.names(stringr::str_remove_all(symbol, "[\\^=]"))) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    na.omit()
  
  ticker_y_adj <- make.names(stringr::str_remove_all(ticker_y, "[\\^=]"))
  ticker_x_adj <- make.names(stringr::str_remove_all(ticker_x, "[\\^=]"))
  
  # --- 2. Configuração do Loop Walk-Forward ---
  total_days <- nrow(prices_wide_full)
  # Define os pontos de início para cada janela de teste (out-of-sample)
  start_points_oos <- seq(from = in_sample_days + 1, to = total_days, by = out_of_sample_days)
  
  all_oos_results <- list() # Lista para armazenar os resultados de cada janela OOS
  
  cli_progress_bar("Executando análise Walk-Forward", total = length(start_points_oos))
  
  for (i in seq_along(start_points_oos)) {
    start_oos <- start_points_oos[i]
    end_oos <- min(start_oos + out_of_sample_days - 1, total_days)
    
    # Define a janela de treino (in-sample) que precede a janela de teste
    end_is <- start_oos - 1
    start_is <- max(1, end_is - in_sample_days + 1)
    
    in_sample_data <- prices_wide_full[start_is:end_is, ]
    out_of_sample_data <- prices_wide_full[start_oos:end_oos, ]
    
    # --- 3. Treino (In-Sample) ---
    regression_formula <- as.formula(paste(ticker_y_adj, "~", ticker_x_adj))
    ratio_model <- lm(regression_formula, data = in_sample_data)
    
    beta_is <- coef(ratio_model)[2]
    
    in_sample_spread <- in_sample_data[[ticker_y_adj]] - beta_is * in_sample_data[[ticker_x_adj]]
    spread_mean_is <- mean(in_sample_spread, na.rm = TRUE)
    spread_sd_is <- sd(in_sample_spread, na.rm = TRUE)
    
    # --- 4. Teste (Out-of-Sample) ---
    # Aplica os parâmetros de treino nos dados de teste
    signals_oos <- out_of_sample_data %>%
      mutate(
        spread = .[[ticker_y_adj]] - beta_is * .[[ticker_x_adj]],
        z_score = (spread - spread_mean_is) / spread_sd_is
      ) %>%
      mutate(
        entry_signal = case_when(
          z_score > entry_z & lag(z_score, default = 0) <= entry_z ~ -1,
          z_score < -entry_z & lag(z_score, default = 0) >= -entry_z ~ 1,
          TRUE ~ 0
        )
      )
    
    # Lógica de posições (copiada e adaptada da função original)
    positions_logic_oos <- signals_oos %>%
      mutate(trade_id = cumsum(entry_signal != 0)) %>%
      group_by(trade_id) %>%
      mutate(days_in_pos = if_else(trade_id == 0, 0, row_number())) %>%
      mutate(
        initial_pos = first(entry_signal),
        exit_long = (initial_pos == 1 & z_score >= -exit_z),
        exit_short = (initial_pos == -1 & z_score <= exit_z),
        stop_loss = !is.na(stop_z) & abs(z_score) >= stop_z,
        max_hold = !is.na(max_hold_days) & days_in_pos >= max_hold_days,
        exit_signal = exit_long | exit_short | stop_loss | max_hold
      ) %>%
      ungroup()
    
    final_positions_oos <- positions_logic_oos %>%
      group_by(trade_id) %>%
      mutate(exit_day_index = which(exit_signal)[1]) %>%
      mutate(
        position = if_else(
          trade_id == 0, 0,
          if_else(!is.na(exit_day_index) & row_number() >= exit_day_index, 0, first(entry_signal))
        )
      ) %>%
      ungroup()
      
    # Cálculo de retornos para a janela OOS
    backtest_results_oos <- final_positions_oos %>%
      mutate(
        ret_y = TTR::ROC(!!sym(ticker_y_adj), type = "discrete"),
        ret_x = TTR::ROC(!!sym(ticker_x_adj), type = "discrete")
      ) %>%
      mutate(spread_ret = ret_y - beta_is * ret_x) %>%
      mutate(position = lag(position, default = 0)) %>%
      mutate(
        gross_ret = position * spread_ret,
        turnover = abs(position - lag(position, default = 0)),
        costs = turnover * (1 + abs(beta_is)) * (tc_bps / 10000),
        net_ret = gross_ret - costs
      ) %>%
      filter(!is.na(net_ret))
      
    all_oos_results[[i]] <- backtest_results_oos
    cli_progress_update()
  }
  
  cli_progress_done()
  
  # --- 5. Consolidação e Visualização ---
  message("Consolidando resultados e gerando gráficos...")
  
  final_results <- bind_rows(all_oos_results) %>%
    mutate(equity_curve = cumprod(1 + net_ret))
    
  p_equity <- ggplot(final_results, aes(x = date, y = equity_curve)) +
    geom_line(color = "darkblue", linewidth = 1) +
    labs(
      title = "Curva de Capital (Out-of-Sample Walk-Forward)",
      subtitle = paste(ticker_y, "vs", ticker_x),
      x = "Data",
      y = "Patrimônio"
    ) +
    theme_tq()
    
  p_zscore_pos <- ggplot(final_results, aes(x = date)) +
    geom_line(aes(y = z_score), color = "gray50") +
    geom_hline(yintercept = c(entry_z, -entry_z), color = "red", linetype = "dashed") +
    geom_hline(yintercept = c(exit_z, -exit_z), color = "blue", linetype = "dotted") +
    geom_line(aes(y = position * entry_z), color = "purple", linewidth = 1) +
    labs(
      title = "Z-score e Posição (Out-of-Sample)",
      subtitle = "Linha Roxa = Posição (Escalonada)",
      x = "Data",
      y = "Z-score"
    ) +
    theme_tq()
    
  return(
    list(
      results_df = final_results,
      plot_equity = p_equity,
      plot_zscore_pos = p_zscore_pos
    )
  )
}

# Exemplo de uso: testando Ibovespa vs. S&P 500
# Treinando com dados de ~1 ano (252 dias) e testando no trimestre seguinte (63 dias)

walkforward_test <- pairs_walkforward(
  ticker_y = "^BVSP",
  ticker_x = "^GSPC",
  from_date = as.Date("2000-01-01"),
  to_date = as.Date("2024-01-01"),
  in_sample_days = 252,
  out_of_sample_days = 63,
  entry_z = 2.0,
  exit_z = 0.5,
  stop_z = 3.5,
  tc_bps = 5 # Adicionando um custo transacional de 0.05%
)

# Visualizar os resultados
walkforward_test$plot_equity
lwalkforward_test$plot_zscore_pos

# Olhar o dataframe de resultados
# head(walkforward_test$results_df)
