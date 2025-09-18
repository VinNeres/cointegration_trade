# Pairs Trading Strategy Analysis and Backtesting

This repository contains an R project for identifying, statistically validating, and backtesting *Pairs Trading* strategies in the stock market. The goal is to provide a clear and educational framework for implementing one of the most classical statistical arbitrage strategies.

## What is Pairs Trading?

*Pairs Trading* is a **market-neutral** investment strategy that seeks to profit from temporary mispricings in the relationship between two assets that are historically correlated.

The logic is simple:
1.  **Identify a Pair:** Find two assets whose prices “move together” over time.
2.  **Monitor the Spread:** When prices deviate abnormally, one asset becomes “expensive” relative to its pair.
3.  **Bet on Convergence:** The trader goes *short* on the expensive asset and *long* on the cheap one.
4.  **Realize Profit:** When the price relationship reverts to its historical mean, positions are closed, yielding profit regardless of overall market direction.

*Analogy — The Drunk and the Dog: their individual paths are random (non-stationary), but the distance between them (the leash, or the “spread”) is stable and mean-reverting (stationary).*

---

## The Statistical Foundation: Cointegration

Simple correlation is not sufficient to guarantee that two assets form a good pair. Stock prices are typically **non-stationary** (they follow a random walk). The statistical foundation of Pairs Trading is the concept of **cointegration**.

Two non-stationary series, $Y_t$ and $X_t$, are said to be cointegrated if there exists a linear combination between them that is **stationary**:

$$
Z_t = Y_t - \beta X_t
$$

Where $Z_t$ is the **spread** (or residual) and $\beta$ is the **hedge ratio**. If $Z_t$ is stationary, it implies a long-run equilibrium relationship between $Y_t$ and $X_t$, and any deviation from that equilibrium tends to be corrected.

### Cointegration Tests Used

To validate a pair, this project employs two main tests:

1.  **Engle-Granger Two-Step Test:**
    -   **Step 1:** Estimate the long-run relationship through a simple linear regression:

        $$
        Y_t = \alpha + \beta X_t + \epsilon_t
        $$

    -   **Step 2:** Extract the regression residuals, $\hat{\epsilon}_t$, and test them for stationarity using the **Augmented Dickey-Fuller (ADF) Test**. If residuals are stationary (p-value < 0.05), the assets are considered cointegrated.

2.  **Johansen Test:**
    -   A more robust approach that models the assets as a system (Vector Auto-Regression - VAR) and determines the number of cointegration relationships. It is especially useful when analyzing more than two assets.

---

## Strategy Workflow

The workflow implemented in this code follows these steps:

1.  **Pair Selection:** Collect historical price data for candidate assets.
2.  **Statistical Validation:**
    -   Estimate the hedge ratio ($\beta$) via linear regression.
    -   Compute the spread (regression residuals).
    -   Apply cointegration tests (Engle-Granger and Johansen) to validate the pair.
3.  **Signal Generation:**
    -   Normalize the spread using the **Z-score** to create a standardized measure of deviation:

        $$
        \text{Z-score}_t = \frac{\text{Spread}_t - \text{RollingMean}(\text{Spread})}{\text{RollingStdDev}(\text{Spread})}
        $$
        
    -   Trading signals are generated based on Z-score thresholds (e.g., enter at ±2.0, exit at 0).
4.  **Backtesting:**
    -   Perform historical simulation of the strategy by applying the trading signals to price data.
    -   Calculate daily returns and build the equity curve.
5.  **Performance Analysis:**
    -   Compute key performance metrics:
        -   Annualized Return
        -   Annualized Volatility
        -   Sharpe Ratio
        -   Maximum Drawdown
        -   Hit Rate (Win Rate)
