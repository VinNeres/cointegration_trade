library(tidyquant)
library(tidyverse)
library(TTR)

tickers <- c("ITUB4.SA", "BBDC4.SA")
end_date <- as.Date("2025-08-01")

prices <- tq_get(
  x = tickers,
  get = "stock.prices",
  from = as.Date("2018-01-01"),
  to = end_date
) %>%
  select(symbol, date, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  as_tibble()

prices %>%
  ggplot() +
  geom_line(aes(x = date, y = BAC), color = "red") +
  geom_line(aes(x = date, y = PNC), color = "blue") +
  theme_minimal()

ratio_model <- lm(BBDC4.SA ~ ITUB4.SA, data = prices)
summary(ratio_model)
ratio <- residuals(ratio_model)
plot(ratio, type = "l")
mean(ratio)

summary(urca::ur.df(ratio, type = "none", selectlags = "AIC"))


df <- tibble(
  date = prices$date,
  spread = ratio,
  ma = runMean(ratio, n = 60),
  sd = runSD(ratio, n = 60),
  zscore = (ratio - ma) / sd
)

entry_z <- 2.0
exit_z <- 0.5
z_window <- 60
max_hold_days <- 20
stop_z <- 3.5
tc_bps <- 0.0

beta <- ratio_model$coefficients[2]
const <- ratio_model$coefficients[1]

# Spread and z-score
zscore <- tibble(
  date = prices$date,
  spread = ratio,
  ma = runMean(ratio, n = 60),
  sd = runSD(ratio, n = 60),
  zscore = (ratio - ma) / sd
) %>%
  mutate(
    operation = case_when(
      zscore > entry_z ~ -1, # short spread
      zscore < entry_z ~ 1, # long spread
      abs(zscore) <= exit_z ~ 0 # exit
    )
  )

pos_loop <- numeric(nrow(zscore))
cur <- 0
days_in_pos <- 0
for (i in 1:nrow(zscore)) {
  zt <- as.numeric(zscore$zscore[i])
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

pos <- zscore %>%
  mutate(signal = pos_loop)

exec_pos <- lag(pos, 1)
exec_pos <- exec_pos %>%
  select(!date) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

ry <- TTR::ROC(prices$ITUB4.SA, type = "discrete")[-1]
rx <- TTR::ROC(prices$BBDC4.SA, type = "discrete")[-1]
spread_ret <- ry - beta * rx

merged_data <- exec_pos %>%
  mutate(
    spread_ret = c(0, spread_ret)
  )

gross_ret <- merged_data[, 6] * merged_data[, 7]

dpos <- merged_data$signal - lag(merged_data$signal)
dpos[1] <- merged_data$signal[1]
turnover <- abs(dpos) * (1.0 + abs(beta))
costs <- turnover * (tc_bps / 10000.0)
net_ret <- gross_ret - costs

equity <- cumprod(1 + net_ret)

plot(equity$signal, type = "l")
