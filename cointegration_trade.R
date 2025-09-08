# ==============================================================================
#  STEP 0: PREREQUISITES AND SETUP
# ==============================================================================

# --- Load Libraries ---
# Make sure you have the utility functions from the previous conversion loaded
source("utils.R") # Assumes the previous R script is saved as utils.R

# Also ensure these are loaded
library(quantmod)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(logger)
library(patchwork) # For combining plots
library(ggcorrplot) # For the heatmap
library(gridExtra) # For tables in the PDF

# --- Setup Folders and Logging ---
if (!dir.exists("data")) {
  dir.create("data")
}
if (!dir.exists("images")) {
  dir.create("images")
}

log_layout(layout_glue_colors)
log_info("Environment ready.")

# --- Optional Debug Toggle ---
DEBUG <- FALSE

# ==============================================================================
#  STEP 1: PARAMETERS
# ==============================================================================
tickers <- c("ITUB4.SA", "BBAS3.SA")
start_date <- "2010-01-01"
end_date <- "2025-08-01" # Using a slightly earlier date for reproducibility

# --- Strategy Parameters ---
entry_z <- 2.0
exit_z <- 0.5
z_window <- 60
tc_bps <- 0.0
max_hold_days <- 20
stop_z <- 3.5
use_ewma <- FALSE
ewma_halflife <- NULL
use_intercept <- TRUE

log_info("Universe size: {length(tickers)}; Date range: {start_date} to {end_date}")

# ==============================================================================
#  STEP 2: FETCH PRICE DATA
# ==============================================================================
prices <- fetch_data(tickers, start_date, end_date, save_path = "data/prices.csv")
log_info("Downloaded {nrow(prices)} rows for {ncol(prices)} tickers.")
print(tail(prices))

# ==============================================================================
#  STEP 3: COINTEGRATION TESTS
# ==============================================================================
coint_results <- find_cointegrated_pairs(prices, significance = 0.05, min_obs = 252)
pval_matrix <- coint_results$pval_matrix
pairs <- coint_results$significant_pairs

log_info("\nCointegrated Pairs (p < 0.05):")
if (length(pairs) > 0) {
  for (p in pairs) {
    log_info("{p$y} & {p$x} | p-value = {format(p$p_value, digits=4)}")
  }
} else {
  stop("No cointegrated pairs found. Try expanding tickers or date range.")
}

# --- Keep the top pair for the single-pair demo ---
top_pair <- c(pairs[[1]]$y, pairs[[1]]$x)
log_info("\nSelected pair: {paste(top_pair, collapse='-')}")

# ==============================================================================
#  STEP 4: SINGLE-PAIR BACKTEST & PLOTS
# ==============================================================================
single_backtest <- backtest_pairs(
  prices,
  top_pair,
  entry_z = entry_z,
  exit_z = exit_z,
  z_window = z_window,
  use_ewma = use_ewma,
  ewma_halflife = ewma_halflife,
  max_hold_days = max_hold_days,
  stop_z = stop_z,
  execute_next_bar = TRUE,
  tc_bps = tc_bps,
  use_intercept = use_intercept
)

metrics <- performance_metrics(single_backtest$ret)
log_info("\nSingle-Pair Performance Report:")
for (k in names(metrics)) {
  log_info("{k}: {format(metrics[[k]], digits=4)}")
}

# --- Plot Equity Curve ---
equity_df <- tibble(
  Date = index(single_backtest$equity),
  Equity = coredata(single_backtest$equity)
)
p_equity <- ggplot(equity_df, aes(x = Date, y = Equity)) +
  geom_line() +
  labs(title = paste("Pairs Trading |", top_pair[1], "vs", top_pair[2])) +
  theme_minimal()
print(p_equity)

# --- Plot Z-Score ---
plot_zscore(single_backtest["zscore"], entry_z = entry_z, exit_z = exit_z, title = paste("Spread Z-score:", top_pair[1], "/", top_pair[2]))

# ==============================================================================
#  STEP 5: MULTI-PAIR PORTFOLIO (BATCH BACKTEST)
# ==============================================================================

# --- Helper function for batch backtesting ---
batch_backtest <- function(
  prices,
  pairs,
  entry_z,
  exit_z,
  z_window = 60,
  tc_bps = 0.0,
  max_hold_days = 20,
  stop_z = 3.5,
  use_intercept = TRUE
) {
  results_list <- list()
  returns_list <- list()

  for (p_info in pairs) {
    pair_tickers <- c(p_info$y, p_info$x)
    bt <- backtest_pairs(prices, pair_tickers, entry_z, exit_z, z_window, max_hold_days = max_hold_days, stop_z = stop_z, tc_bps = tc_bps, use_intercept = use_intercept)

    m <- performance_metrics(bt$ret)
    m$Pair <- paste(pair_tickers, collapse = "-")
    m$p_value <- p_info$p_value

    results_list <- append(results_list, list(as.data.frame(m)))

    # Name the returns series for merging
    ret_series <- bt$ret
    colnames(ret_series) <- m$Pair
    returns_list <- append(returns_list, list(ret_series))
  }

  metrics_df <- bind_rows(results_list) %>%
    select(Pair, p_value, everything()) %>%
    arrange(desc(Sharpe))

  returns_df <- do.call(merge, returns_list)
  returns_df[is.na(returns_df)] <- 0

  return(list(metrics_df = metrics_df, returns_df = returns_df))
}

# --- Helper function for rolling Sharpe ---
rolling_sharpe <- function(returns, window = 126, freq = 252) {
  # Use PerformanceAnalytics::rollapply for this
  roll_sharpe <- zoo::rollapply(returns, width = window, FUN = function(x) SharpeRatio.annualized(x, scale = freq), align = "right", fill = NA)
  return(roll_sharpe)
}

# --- Run the batch backtest ---
portfolio_results <- batch_backtest(
  prices,
  pairs,
  entry_z,
  exit_z,
  z_window,
  tc_bps,
  max_hold_days = max_hold_days,
  stop_z = stop_z,
  use_intercept = use_intercept
)
metrics_df <- portfolio_results$metrics_df
returns_df <- portfolio_results$returns_df

print(head(metrics_df, 10))

# --- Save metrics ---
metrics_path <- "data/pair_metrics.csv"
write.csv(metrics_df, metrics_path, row.names = FALSE)
log_info("Metrics saved to {metrics_path}")

# --- Create equal-weight portfolio ---
portfolio_returns <- xts(rowMeans(returns_df), order.by = index(returns_df))
portfolio_equity <- cumprod(1 + portfolio_returns)

# --- Plot Portfolio Equity ---
portfolio_equity_df <- data.frame(Date = index(portfolio_equity), Equity = coredata(portfolio_equity))
p_portfolio_equity <- ggplot(portfolio_equity_df, aes(x = Date, y = Equity)) +
  geom_line(color = "blue") +
  labs(title = "Multi-Pair Pairs Trading Portfolio (Equal-Weight)", x = "Date", y = "Cumulative Return (x)") +
  theme_minimal()
print(p_portfolio_equity)

# --- Plot Rolling Sharpe ---
rs <- rolling_sharpe(portfolio_returns, window = 126)
rs_df <- na.omit(data.frame(Date = index(rs), Sharpe = coredata(rs)))
p_rolling_sharpe <- ggplot(rs_df, aes(x = Date, y = Sharpe)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Portfolio Rolling Sharpe Ratio (126d)") +
  theme_minimal()
print(p_rolling_sharpe)

# --- Final Portfolio Metrics ---
portfolio_metrics <- performance_metrics(portfolio_returns)
log_info("\nPortfolio Performance:")
for (k in names(portfolio_metrics)) {
  log_info("{k}: {format(portfolio_metrics[[k]], digits=4)}")
}

# ==============================================================================
#  STEP 6: CREATE PDF REPORT
# ==============================================================================
log_info("Generating PDF report...")

# --- Heatmap Plot ---
# Using ggcorrplot for a ggplot-based heatmap
p_heatmap <- ggcorrplot(
  t(pval_matrix), # transpose to match python output
  type = "upper",
  show.diag = FALSE,
  lab = TRUE,
  lab_size = 3,
  colors = c("firebrick", "white", "steelblue")
) +
  labs(title = "Cointegration P-Value Heatmap (Engle-Granger)", fill = "p-value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("images/heatmap_r.png", plot = p_heatmap, width = 8, height = 7)

# --- Start PDF device ---
pdf("Quant_Research_Report_R.pdf", width = 11, height = 8.5)

# --- Page 1: Overview ---
overview_text <- paste(
  "Quant Research Report — Pairs Trading\n\n",
  "Data Period:",
  start(prices),
  "to",
  end(prices),
  "\n",
  "Number of Tickers:",
  ncol(prices),
  "\n",
  "Cointegrated Pairs Found:",
  length(pairs),
  "\n\n",
  "Top 5 Pairs by Sharpe Ratio:\n",
  capture.output(print(metrics_df %>% select(Pair, Sharpe) %>% head(5)))[2:7],
  collapse = "\n"
)
grid.newpage()
grid.text(overview_text, x = 0.05, y = 0.95, just = c("left", "top"), gp = gpar(fontfamily = "mono", fontsize = 10))

# --- Page 2: Full Metrics Table ---
grid.newpage()
grid.table(metrics_df %>% mutate(across(where(is.numeric), ~ round(., 4))), rows = NULL) # grid.table is a simple way to plot a dataframe

# --- Page 3: Portfolio Equity Curve ---
print(p_portfolio_equity)

# --- Page 4: Rolling Sharpe ---
print(p_rolling_sharpe)

# --- Page 5: Returns Histogram ---
portfolio_returns_df <- data.frame(Returns = coredata(portfolio_returns))
p_hist <- ggplot(portfolio_returns_df, aes(x = Returns)) +
  geom_histogram(bins = 50, fill = "grey", color = "black") +
  labs(title = "Distribution of Portfolio Daily Returns", x = "Daily Return", y = "Frequency") +
  theme_minimal()
print(p_hist)

# --- Page 6: Portfolio Summary ---
portfolio_summary_df <- as.data.frame(portfolio_metrics) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")
grid.newpage()
grid.table(portfolio_summary_df %>% mutate(Value = round(Value, 4)), rows = NULL)

# --- Subsequent Pages: Individual Pair Plots ---
for (p_info in pairs) {
  pair_tickers <- c(p_info$y, p_info$x)
  bt <- backtest_pairs(prices, pair_tickers, entry_z, exit_z, z_window, max_hold_days = max_hold_days, stop_z = stop_z, tc_bps = tc_bps, use_intercept = use_intercept)

  # Equity Plot
  eq_df <- data.frame(Date = index(bt$equity), Value = coredata(bt$equity))
  p1 <- ggplot(eq_df, aes(x = Date, y = Value)) +
    geom_line(color = "blue") +
    labs(title = paste("Equity Curve:", paste(pair_tickers, collapse = "-"))) +
    theme_minimal()

  # Z-Score Plot
  z_df <- data.frame(Date = index(bt$zscore), Value = coredata(bt$zscore))
  p2 <- ggplot(z_df, aes(x = Date, y = Value)) +
    geom_line(color = "purple") +
    geom_hline(yintercept = c(entry_z, -entry_z), color = "red", linetype = "dashed") +
    geom_hline(yintercept = c(exit_z, -exit_z), color = "grey", linetype = "dashed") +
    labs(title = paste("Z-score:", paste(pair_tickers, collapse = "-"))) +
    theme_minimal()

  # Combine plots onto one page
  combined_plot <- p1 / p2 # Using patchwork to stack plots
  print(combined_plot)
}
