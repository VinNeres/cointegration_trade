# utils.py
"""
Utility functions for:
- Data fetching from Yahoo Finance
- Cointegration testing
- Pairs trading backtest
- Performance metrics
- Plotting helpers
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import numpy as np
import pandas as pd
import yfinance as yf
import statsmodels.api as sm
from statsmodels.tsa.stattools import coint
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------
logger = logging.getLogger(__name__)
if not logger.handlers:
    handler = logging.StreamHandler()
    formatter = logging.Formatter("%(asctime)s %(levelname)s %(name)s - %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
logger.setLevel(logging.INFO)

# -----------------------------------------------------------------------------
# Helpers and Config
# -----------------------------------------------------------------------------
@dataclass(frozen=True)
class BacktestConfig:
    entry_z: float = 2.0
    exit_z: float = 0.5
    z_window: int = 60
    use_ewma: bool = False
    ewma_halflife: Optional[float] = None
    max_hold_days: Optional[int] = 20
    stop_z: Optional[float] = 3.5
    execute_next_bar: bool = True
    tc_bps: float = 0.0  # Transaction costs per side in bps (default OFF)
    use_intercept: bool = True  # Include intercept in hedge ratio


def _align_series(a: pd.Series, b: pd.Series) -> Tuple[pd.Series, pd.Series]:
    """Align two price series on intersection of dates and drop NaNs."""
    df = pd.concat([a.rename("a"), b.rename("b")], axis=1).dropna()
    return df["a"], df["b"]


def hedge_ratio_ols(y: pd.Series, x: pd.Series, add_const: bool = True) -> Tuple[float, float]:
    """
    OLS hedge ratio for y ~ const + beta*x.
    Returns (beta, const).
    """
    y_al, x_al = _align_series(y, x)
    if add_const:
        X = sm.add_constant(x_al.astype(float))
    else:
        X = x_al.astype(float)
    model = sm.OLS(y_al.astype(float), X).fit()
    if add_const:
        const = float(model.params.get("const", 0.0))
        beta = float([v for k, v in model.params.items() if k != "const"][0])
    else:
        const = 0.0
        beta = float(model.params[0])
    return beta, const


def rolling_zscore(x: pd.Series, window: int) -> pd.Series:
    """Rolling z-score with lookahead-safe window."""
    m = x.rolling(window).mean()
    s = x.rolling(window).std(ddof=1)
    return (x - m) / s


def ewma_zscore(x: pd.Series, halflife: float) -> pd.Series:
    """EWMA z-score (half-life based)."""
    mean = x.ewm(halflife=halflife, adjust=False).mean()
    var = x.ewm(halflife=halflife, adjust=False).var(bias=False)
    std = np.sqrt(var)
    return (x - mean) / std


# -----------------------------------------------------------------------------
# Fetch Adjusted Close Prices
# -----------------------------------------------------------------------------
def fetch_data(tickers: Sequence[str], start: str, end: str, save_path: Optional[str] = None) -> pd.DataFrame:
    """
    Download adjusted close prices from Yahoo Finance for given tickers.
    Uses auto_adjust=True to ensure prices are split/dividend adjusted.
    Returns a clean DataFrame with tickers as columns.
    """
    if not tickers:
        raise ValueError("tickers must be a non-empty sequence.")

    logger.info("Fetching data for %d tickers from %s to %s", len(tickers), start, end)
    data = yf.download(
        list(tickers),
        start=start,
        end=end,
        auto_adjust=True,  # Adjusts OHLC automatically
        progress=False,
        group_by="column",  # ensures a single-level column naming when possible
        threads=True,
    )

    if data.empty:
        raise ValueError(f"No data fetched for tickers: {tickers}. Check symbols or date range.")

    # Only keep the Close column (already adjusted)
    if isinstance(data.columns, pd.MultiIndex):
        if "Close" in data.columns.levels[0]:
            prices = data["Close"]
        else:
            # Fallback: try to locate a Close-like field, else take the first level
            lvl0 = data.columns.levels[0]
            key = "Adj Close" if "Adj Close" in lvl0 else lvl0[0]
            prices = data[key]
    else:
        prices = data["Close"] if "Close" in data.columns else data

    prices = prices.sort_index()
    prices = prices.dropna(how="all").ffill()
    # Ensure float dtype
    prices = prices.astype(float)

    # Save to CSV if requested
    if save_path:
        prices.to_csv(save_path)
        logger.info("Saved prices to %s", save_path)

    return prices


# -----------------------------------------------------------------------------
# Find Cointegrated Pairs (Engle–Granger)
# -----------------------------------------------------------------------------
def find_cointegrated_pairs(
    data: pd.DataFrame,
    significance: float = 0.05,
    min_obs: int = 252,
    trend: str = "c",  # 'c' (const), 'ct' (const+trend), 'ctt', 'nc'
) -> Tuple[pd.DataFrame, List[Tuple[str, str, float]]]:
    """
    Tests all pairs for cointegration using Engle–Granger two-step method.
    Returns (pval_matrix, pairs_list) where:
      - pval_matrix is a DataFrame of p-values (upper triangle filled).
      - pairs_list is a list of (ticker_i, ticker_j, pval) sorted by p-value.

    Notes:
    - Drops NaNs pairwise and requires at least min_obs observations.
    - trend controls deterministic terms in the cointegration test.
    """
    cols = list(data.columns)
    n = len(cols)
    pval_matrix = pd.DataFrame(np.ones((n, n)), index=cols, columns=cols)
    pairs: List[Tuple[str, str, float]] = []

    logger.info("Scanning %d series for cointegration (EG, trend=%s)", n, trend)
    for i in range(n):
        for j in range(i + 1, n):
            y, x = _align_series(data[cols[i]], data[cols[j]])
            if len(y) < min_obs:
                continue
            try:
                _, pval, _ = coint(y, x, trend=trend)
            except Exception as e:
                logger.warning("coint failed for (%s, %s): %s", cols[i], cols[j], e)
                pval = 1.0
            pval_matrix.loc[cols[i], cols[j]] = pval
            if pval < significance:
                pairs.append((cols[i], cols[j], float(pval)))

    pairs.sort(key=lambda t: t[2])
    return pval_matrix, pairs


# -----------------------------------------------------------------------------
# Backtest Pairs Trading
# -----------------------------------------------------------------------------
def backtest_pairs(
    prices: pd.DataFrame,
    pair: Tuple[str, str],
    entry_z: float = 2.0,
    exit_z: float = 0.5,
    z_window: int = 60,
    use_ewma: bool = False,
    ewma_halflife: Optional[float] = None,
    max_hold_days: Optional[int] = 20,
    stop_z: Optional[float] = 3.5,
    execute_next_bar: bool = True,
    tc_bps: float = 0.0,  # Transaction costs per side in bps (default OFF)
    use_intercept: bool = True,
) -> Tuple[pd.Series, pd.Series, pd.Series]:
    """
    Simple pairs trading backtest:
    - Hedge ratio via OLS (y ~ const + beta*x)
    - Spread = y - (const + beta*x)
    - Z-score: rolling window (no lookahead) or EWMA if use_ewma
    - Entry: z > +entry_z → Short spread
    - Entry: z < -entry_z → Long spread
    - Exit: |z| <= exit_z → Close position
    - Optional: max_hold_days and stop_z
    - Execution: positions shifted by 1 bar if execute_next_bar
    - Transaction costs: tc_bps per side applied on position changes; default 0.0 (off)

    Returns:
      equity: cumulative equity curve from net returns (starts at 1.0)
      ret: daily net strategy returns
      zscore: z-score of the spread
    """
    s1 = prices[pair[0]].rename("y").astype(float)
    s2 = prices[pair[1]].rename("x").astype(float)
    y, x = _align_series(s1, s2)

    # Hedge ratio (static)
    beta, const = hedge_ratio_ols(y, x, add_const=use_intercept)

    # Spread and z-score
    spread = y - (const + beta * x)
    if use_ewma and ewma_halflife is not None:
        z = ewma_zscore(spread, halflife=ewma_halflife)
    else:
        z = rolling_zscore(spread, window=z_window)

    # Generate positions with neutral band, max-hold, and stop
    pos = []
    cur = 0
    days_in_pos = 0
    for t, zt in z.items():
        if np.isnan(zt):
            pos.append(0 if not pos else pos[-1])
            continue

        if cur == 0:
            if zt > entry_z:
                cur = -1  # short spread
                days_in_pos = 1
            elif zt < -entry_z:
                cur = 1   # long spread
                days_in_pos = 1
        else:
            days_in_pos += 1
            if abs(zt) <= exit_z:
                cur = 0
                days_in_pos = 0
            elif (stop_z is not None) and (abs(zt) >= stop_z):
                cur = 0
                days_in_pos = 0
            elif (max_hold_days is not None) and (days_in_pos >= max_hold_days):
                cur = 0
                days_in_pos = 0

        pos.append(cur)

    pos = pd.Series(pos, index=z.index, name="position").fillna(0).astype(int)

    # Execution: shift positions one bar to avoid lookahead if requested
    exec_pos = pos.shift(1).fillna(0).astype(int) if execute_next_bar else pos

    # Returns of legs
    ry = y.pct_change().fillna(0.0)
    rx = x.pct_change().fillna(0.0)
    spread_ret = ry - beta * rx

    # Strategy returns
    gross_ret = exec_pos * spread_ret

    # Transaction costs on position changes (both legs) — default OFF via tc_bps=0.0
    dpos = exec_pos.diff().fillna(exec_pos).abs()
    turnover_units = dpos * (1.0 + abs(beta))  # approx both legs traded
    costs = turnover_units * (tc_bps / 10000.0)
    net_ret = gross_ret - costs

    equity = (1.0 + net_ret).cumprod().rename("equity")

    return equity, net_ret.rename("ret"), z.rename("zscore")


# -----------------------------------------------------------------------------
# Performance Metrics
# -----------------------------------------------------------------------------
def performance_metrics(returns: pd.Series, freq: int = 252) -> Dict[str, float]:
    """
    Compute metrics: CAGR, Annualized Mean, Volatility, Sharpe, Sortino, Max Drawdown, Win Rate.
    Assumes 'returns' are periodic percentage returns (e.g., daily).
    """
    rets = returns.dropna()
    if rets.empty:
        return {
            "CAGR": 0.0,
            "Annualized Mean": 0.0,
            "Annualized Volatility": 0.0,
            "Sharpe Ratio": 0.0,
            "Sortino Ratio": 0.0,
            "Max Drawdown": 0.0,
            "Win Rate": 0.0,
        }

    ann_mean = rets.mean() * freq
    ann_vol = rets.std(ddof=1) * np.sqrt(freq)
    sharpe = float(ann_mean / ann_vol) if ann_vol > 0 else 0.0

    downside = rets.copy()
    downside[downside > 0] = 0.0
    downside_vol = downside.std(ddof=1) * np.sqrt(freq)
    sortino = float(ann_mean / downside_vol) if downside_vol > 0 else 0.0

    cum = (1.0 + rets).cumprod()
    peak = cum.cummax()
    dd = (cum / peak) - 1.0
    max_dd = float(dd.min())

    total_return = float(cum.iloc[-1] - 1.0)
    years = len(rets) / float(freq)
    cagr = float((1.0 + total_return) ** (1.0 / years) - 1.0) if years > 0 else 0.0

    win_rate = float((rets > 0).mean())

    return {
        "CAGR": cagr,
        "Annualized Mean": float(ann_mean),
        "Annualized Volatility": float(ann_vol),
        "Sharpe Ratio": sharpe,
        "Sortino Ratio": sortino,
        "Max Drawdown": max_dd,
        "Win Rate": win_rate,
    }


# -----------------------------------------------------------------------------
# Plot Z-Score Bands
# -----------------------------------------------------------------------------
def plot_zscore(
    zscore: pd.Series,
    entry_z: float = 2.0,
    exit_z: float = 0.5,
    title: str = "Z-score Spread",
    ax: Optional[plt.Axes] = None,
    show: bool = True,
):
    if ax is None:
        fig, ax = plt.subplots(figsize=(12, 5))
    ax.plot(zscore, label="Z-score")
    ax.axhline(entry_z, color="red", linestyle="--", label="Upper Threshold")
    ax.axhline(-entry_z, color="green", linestyle="--", label="Lower Threshold")
    ax.axhline(exit_z, color="grey", linestyle="--", label="Exit Threshold")
    ax.axhline(-exit_z, color="grey", linestyle="--")
    ax.set_title(title)
    ax.legend()
    if show:
        plt.show()