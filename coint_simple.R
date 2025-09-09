library(tidyquant)
library(tidyverse)
library(TTR)

tickers <- c("ITUB4.SA","BBAS3.SA")
start_date <- as.Date("2012-01-01")
end_date <- as.Date("2025-08-01")

prices <- tq_get(
  x = tickers,
  get = "stock.prices",
  from = start_date,
  to = end_date
) %>%
  select(symbol, date, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  as_tibble()

prices %>% ggplot() +
  geom_line(aes(x = date, y = ITUB4.SA), color = "red") +
  geom_line(aes(x = date, y = BBAS3.SA), color = "blue") +
  theme_minimal()
  
ratio_model <- lm(ITUB4.SA ~ BBAS3.SA, data = prices)
summary(ratio_model)
ratio <- residuals(ratio_model)
plot(ratio, type = "l")
mean(ratio)

vars::VARselect(
  prices[, c("ITUB4.SA", "BBAS3.SA")],
  lag.max = 25,
  type = "trend"
)

summary(urca::ur.df(ratio, selectlags = "BIC", type = "none"))
tseries::adf.test(ratio, k = 1)
summary(urca::ca.jo(prices[,c("ITUB4.SA","BBAS3.SA")], ecdet = "trend", K = 8))
aTSA::coint.test(prices$ITUB4.SA, prices$BBAS3.SA, nlag = 8)

zscore <- tibble(
  ma = runMean(ratio, n = 60),
  sd = runSD(ratio, n = 60),
  zscore = (ratio - ma) / sd
) %>%
  mutate(
    signal = case_when(
      zscore > 2 ~ "sell_spread",
      zscore < -2 ~ "buy_spread",
      TRUE ~ NA_character_
    ),
    exit = case_when(
      abs(zscore) < 0 ~ "exit_profit",
      zscore > 3 ~ "exit_loss",
      zscore < -3 ~ "exist_loss",
      TRUE ~ NA_character_
    )
  )

