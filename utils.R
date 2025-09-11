# utils.R
#
# Utility functions in R for:
# - Data fetching from Yahoo Finance
# - Cointegration testing
# - Pairs trading backtest
# - Performance metrics
# - Plotting helpers

# ==============================================================================
#  STEP 0: REQUIRED PACKAGES
# ==============================================================================
# install.packages(c("quantmod", "urca", "dplyr", "tidyr", "zoo", "ggplot2", "PerformanceAnalytics", "purrr", "logger"))
library(quantmod)
library(urca)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(PerformanceAnalytics)
library(purrr)
library(logger)

# ==============================================================================
#  1. Logging and Configuration
# ==============================================================================

# R's 'logger' package is a good equivalent to Python's logging
log_layout(layout_glue_colors)
log_info("Pairs Trading Utility Functions Loaded")

# R does not have a direct equivalent of a frozen dataclass.
# The closest is a named list, which is the standard way to handle configuration.
# We will pass these parameters as arguments with default values.

# ==============================================================================
#  2. Helper Functions
# ==============================================================================

# Align two xts/zoo series on their common dates and drop NAs
# @param a An xts/zoo series
# @param b An xts/zoo series
# @return A list containing the two aligned series
align_series <- function(a, b) {
  df <- merge(a, b, join = "inner")
  df <- na.omit(df)
  return(list(a = df[, 1], b = df[, 2]))
}

# Calculate OLS hedge ratio for y ~ const + beta*x
# @param y An xts/zoo series
# @param x An xts/zoo series
# @param add_const Logical, whether to include an intercept
# @return A named list with `beta` and `const`
hedge_ratio_ols <- function(y, x, add_const = TRUE) {
  aligned <- align_series(y, x)
  y_al <- aligned$a
  x_al <- aligned$b

  if (add_const) {
    model <- lm(y_al ~ x_al)
    const <- coef(model)[1]
    beta <- coef(model)[2]
  } else {
    model <- lm(y_al ~ 0 + x_al)
    const <- 0.0
    beta <- coef(model)[1]
  }
  return(list(beta = as.numeric(beta), const = as.numeric(const)))
}

# Calculate rolling z-score
# @param x An xts/zoo series
# @param window The lookback window
# @return An xts/zoo series of z-scores
rolling_zscore <- function(x, window) {
  # Extrair os dados numéricos para evitar problemas de classe
  x_numeric <- as.numeric(coredata(x))

  # Usar zoo::rollapply para média e desvio padrão.
  # O `align = "right"` garante que não haja lookahead bias.
  # `fill = NA` coloca NAs no início, onde não há dados suficientes.
  m <- zoo::rollapply(
    x_numeric,
    width = window,
    FUN = mean,
    align = "right",
    fill = NA
  )
  s <- zoo::rollapply(
    x_numeric,
    width = window,
    FUN = sd,
    align = "right",
    fill = NA
  )

  # Calcular o z-score
  z_numeric <- (x_numeric - m) / s

  # Reconstruir o objeto xts com os mesmos índices de tempo do original
  z_xts <- xts(z_numeric, order.by = index(x))

  return(z_xts)
}

# Calculate EWMA z-score (using TTR package)
# @param x An xts/zoo series
# @param halflife The half-life for the EWMA
# @return An xts/zoo series of z-scores
ewma_zscore <- function(x, halflife) {
  # TTR::EMA é o equivalente R mais próximo para a média móvel exponencial.
  # A relação entre n (períodos) e halflife é: n ≈ halflife / log(2) para um certo tipo de EMA,
  # mas uma aproximação comum é n = (2 / alpha) - 1, onde alpha = 1 - exp(log(0.5) / halflife)
  alpha <- 1 - exp(log(0.5) / halflife)
  n_ema <- (2 / alpha) - 1

  x_numeric <- as.numeric(coredata(x))

  # Calcular a média exponencial
  mean_ema <- TTR::EMA(x_numeric, n = n_ema)

  # A variância/desvio padrão exponencial é mais complexa. Não há uma função direta e simples
  # como a `ewm().var()` do pandas. A abordagem mais comum em R é usar um desvio padrão móvel
  # simples com a mesma janela de tempo equivalente.
  window_equiv <- floor(n_ema)
  std_roll <- zoo::rollapply(
    x_numeric,
    width = window_equiv,
    FUN = sd,
    align = "right",
    fill = NA
  )

  # Calcular o z-score
  z_numeric <- (x_numeric - mean_ema) / std_roll

  # Reconstruir o objeto xts
  z_xts <- xts(z_numeric, order.by = index(x))

  return(z_xts)
}

# ==============================================================================
#  3. Data Fetching
# ==============================================================================

# Fetch adjusted close prices from Yahoo Finance
# @param tickers A character vector of tickers
# @param start Start date "YYYY-MM-DD"
# @param end End date "YYYY-MM-DD"
# @param save_path Optional path to save CSV
# @return An xts object with adjusted close prices
fetch_data <- function(tickers, start, end, save_path = NULL) {
  if (length(tickers) == 0) {
    stop("tickers must be a non-empty vector.")
  }

  log_info("Fetching data for {length(tickers)} tickers from {start} to {end}")

  # quantmod::getSymbols downloads data and stores it in an environment
  data_env <- new.env()
  getSymbols(
    tickers,
    src = "yahoo",
    from = start,
    to = end,
    auto.assign = TRUE,
    env = data_env
  )

  # Extract the Adjusted close price for each ticker and merge
  price_list <- lapply(ls(data_env), function(ticker) {
    Ad(get(ticker, envir = data_env))
  })

  if (length(price_list) == 0) {
    stop("No data fetched for tickers: ", paste(tickers, collapse = ", "))
  }

  prices <- do.call(merge, price_list)
  colnames(prices) <- gsub(".Adjusted", "", colnames(prices))

  prices <- prices[complete.cases(prices), ] # Drop rows with any NAs for simplicity
  prices <- na.locf(prices) # Forward fill any remaining NAs

  if (!is.null(save_path)) {
    write.zoo(prices, file = save_path, sep = ",")
    log_info("Saved prices to {save_path}")
  }

  return(prices)
}

# ==============================================================================
#  4. Cointegration Testing
# ==============================================================================

# Find cointegrated pairs using Engle-Granger test
# @param data An xts object of prices
# @param significance The p-value threshold
# @param min_obs Minimum number of observations required
# @param trend The deterministic trend assumption for the test
# @return A list containing the p-value matrix and a list of significant pairs
find_cointegrated_pairs <- function(
  data,
  significance = 0.05,
  min_obs = 252,
  trend = "c"
) {
  require(urca)
  require(aTSA)

  cols <- colnames(data)
  n <- length(cols)
  pval_matrix <- matrix(1, nrow = n, ncol = n, dimnames = list(cols, cols))
  pairs <- list()

  log_info("Scanning {n} series for cointegration (EG, trend={trend})")

  for (i in 1:n) {
    for (j in (i + 1):n) {
      if (j > n) {
        next
      } # Ensure we don't go out of bounds

      aligned <- align_series(data[, i], data[, j])
      if (nrow(aligned$a) < min_obs) {
        next
      }

      # The urca::ca.jo test is for Johansen, not Engle-Granger.
      # A simple Engle-Granger test is to regress y on x and test residuals for a unit root.
      # Let's use the `egcm` package's test or a manual ADF test on residuals.
      # For simplicity, we'll do it manually.

      y <- aligned$a
      x <- aligned$b

      coint_reg <- lm(y ~ x)
      residuals <- residuals(coint_reg)

      # ADF test on residuals
      adf_test <- ur.df(residuals, type = "none", lags = 1)
      pval <- adf_test@teststat[1] # This is the test stat, not p-value. urca can be tricky.
      # A simpler way is to use a package that provides p-values directly, like `tseries`
      # For this example, let's use tseries::coint.test which is simpler

      tryCatch(
        {
          # Note: tseries coint.test is deprecated, but simpler to show logic
          # A more modern approach might use `aeg` from `urca` or another package
          coint_res <- aTSA::coint.test(as.matrix(y), as.matrix(x), d = 0) # d=0 for I(1) variables
          pval <- coint_res[1, 3]
        },
        error = function(e) {
          log_warn("coint.test failed for ({cols[i]}, {cols[j]}): {e$message}")
          pval <<- 1.0
        }
      )

      pval_matrix[i, j] <- pval
      if (pval < significance) {
        pairs <- append(
          pairs,
          list(list(y = cols[i], x = cols[j], p_value = pval))
        )
      }
    }
  }

  # Sort pairs by p-value
  if (length(pairs) > 0) {
    pairs <- pairs[order(sapply(pairs, `[[`, "p_value"))]
  }

  return(list(pval_matrix = pval_matrix, significant_pairs = pairs))
}


# ==============================================================================
#  5. Backtesting
# ==============================================================================

# Backtest a pairs trading strategy
# @param prices An xts object of prices
# @param pair A character vector of two tickers
# @param ... Other configuration parameters
# @return A list with the equity curve, returns, and z-score series
backtest_pairs <- function(
  prices,
  pair,
  entry_z = 2.0,
  exit_z = 0.5,
  z_window = 60,
  use_ewma = FALSE,
  ewma_halflife = NULL,
  max_hold_days = 20,
  stop_z = 3.5,
  execute_next_bar = TRUE,
  tc_bps = 0.0,
  use_intercept = TRUE
) {
  s1 <- prices[, pair[1]]
  s2 <- prices[, pair[2]]
  aligned <- align_series(s1, s2)
  y <- aligned$a
  x <- aligned$b

  # Hedge ratio (static, has lookahead bias, as noted in Python review)
  hr <- hedge_ratio_ols(y, x, add_const = use_intercept)
  beta <- hr$beta
  const <- hr$const

  # Spread and z-score
  spread <- y - (const + beta * x)
  if (use_ewma && !is.null(ewma_halflife)) {
    z <- ewma_zscore(spread, halflife = ewma_halflife)
  } else {
    z <- rolling_zscore(spread, window = z_window)
  }

  # Generate positions (vectorized approach is faster in R than a loop)
  pos <- xts(matrix(0, nrow = nrow(z), ncol = 1), order.by = index(z))
  pos[z > entry_z] <- -1 # Short spread
  pos[z < -entry_z] <- 1 # Long spread
  pos[abs(z) <= exit_z] <- 0 # Exit

  # Fill forward the positions to hold them
  pos <- na.locf(pos, na.rm = FALSE)
  pos[is.na(pos)] <- 0

  # This vectorized approach doesn't handle max_hold_days or stop_z easily.
  # The loop from the Python code is more explicit for these conditions.
  # Let's replicate the loop for accuracy.
  pos_loop <- numeric(length(z))
  cur <- 0
  days_in_pos <- 0
  for (i in 1:length(z)) {
    zt <- as.numeric(z[i])
    if (is.na(zt)) {
      pos_loop[i] <- if (i == 1) 0 else pos_loop[i - 1]
      next
    }

    if (cur == 0) {
      if (zt > entry_z) {
        cur <- -1
        days_in_pos <- 1
      } else if (zt < -entry_z) {
        cur <- 1
        days_in_pos <- 1
      }
    } else {
      days_in_pos <- days_in_pos + 1
      if (
        abs(zt) <= exit_z ||
          (!is.null(stop_z) && abs(zt) >= stop_z) ||
          (!is.null(max_hold_days) && days_in_pos >= max_hold_days)
      ) {
        cur <- 0
        days_in_pos <- 0
      }
    }
    pos_loop[i] <- cur
  }
  pos <- xts(pos_loop, order.by = index(z))

  # Execution lag
  exec_pos <- if (execute_next_bar) lag(pos, 1) else pos
  exec_pos[is.na(exec_pos)] <- 0

  # Returns
  ry <- TTR::ROC(y, type = "discrete")[-1]
  rx <- TTR::ROC(x, type = "discrete")[-1]
  spread_ret <- ry - beta * rx

  # Align returns and positions
  merged_data <- merge(exec_pos, spread_ret, join = "inner")

  gross_ret <- merged_data[, 1] * merged_data[, 2]

  # Transaction Costs
  dpos <- diff(merged_data[, 1])
  dpos <- c(merged_data[1, 1], dpos) # Add first position
  turnover <- abs(dpos) * (1.0 + abs(beta))
  costs <- turnover * (tc_bps / 10000.0)
  net_ret <- gross_ret - costs

  equity <- cumprod(1 + net_ret)

  return(list(equity = equity, ret = net_ret, zscore = z))
}


# ==============================================================================
#  6. Performance Metrics
# ==============================================================================

# Compute performance metrics using the excellent PerformanceAnalytics package
# @param returns An xts object of returns
# @param freq Annualization factor (e.g., 252 for daily)
# @return A named list of performance metrics
performance_metrics <- function(returns, freq = 252) {
  rets <- na.omit(returns)
  if (length(rets) == 0) {
    return(list(
      CAGR = 0,
      Ann_Mean = 0,
      Ann_Vol = 0,
      Sharpe = 0,
      Sortino = 0,
      Max_DD = 0,
      Win_Rate = 0
    ))
  }

  # Using PerformanceAnalytics functions
  cagr <- as.numeric(Return.annualized(rets, scale = freq, geometric = TRUE))
  ann_mean <- as.numeric(Return.annualized(
    rets,
    scale = freq,
    geometric = FALSE
  ))
  ann_vol <- as.numeric(StdDev.annualized(rets, scale = freq))
  sharpe <- as.numeric(SharpeRatio.annualized(
    rets,
    scale = freq,
    geometric = FALSE
  ))
  sortino <- as.numeric(SortinoRatio(rets))
  max_dd <- as.numeric(maxDrawdown(rets))
  win_rate <- length(rets[rets > 0]) / length(rets)

  return(list(
    CAGR = cagr,
    Ann_Mean = ann_mean,
    Ann_Vol = ann_vol,
    Sharpe = sharpe,
    Sortino = sortino,
    Max_DD = max_dd,
    Win_Rate = win_rate
  ))
}

# ==============================================================================
#  7. Plotting
# ==============================================================================

# Plot a z-score series with bands
# @param zscore An xts object of z-scores
# @param ... Other configuration parameters
plot_zscore <- function(
  zscore,
  entry_z = 2.0,
  exit_z = 0.5,
  title = "Z-score Spread"
) {
  # Convert xts to data.frame for ggplot2
  df_z <- data.frame(
    Date = index(zscore),
    Z = coredata(zscore)
  )

  p <- ggplot(df_z, aes(x = Date, y = Z)) +
    geom_line() +
    geom_hline(yintercept = entry_z, color = "red", linetype = "dashed") +
    geom_hline(yintercept = -entry_z, color = "green", linetype = "dashed") +
    geom_hline(yintercept = exit_z, color = "grey", linetype = "dashed") +
    geom_hline(yintercept = -exit_z, color = "grey", linetype = "dashed") +
    labs(
      title = title,
      y = "Z-score",
      x = "Date"
    ) +
    theme_minimal()

  print(p)
  invisible(p) # Return the plot object
}


ipca <- rbcb::get_series(code = c("ipca" = 433), start_date = "2025-01-01") %>%
  mutate(
    ipca = ipca / 100 + 1,
    ipca_acum = cumprod(ipca)
  )
