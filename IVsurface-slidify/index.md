---
title: "Implied Volatility Surface"
author: "Rafał P."
highlighter: highlight.js
output:
  html_document:
    highlight: espresso
    theme: flatly
job: null
knit: slidify::knit2slides
mode: selfcontained
hitheme: today
subtitle: from S&P 500 options quoted on CBOE
framework: shower
widgets: mathjax
---

Option's price in Black-Scholes model is function
$$C = f(S_t, K, r, d, \sigma, \tau) $$
where $S_t$ is underlying asset, $K$ strike price, $r$ interest rate, $d$ dividend yield, $\sigma$ volatility, $\tau$ time to maturity.

Every variable, except volatility $\sigma$ can be observe in the market...

---

$\dots$but we have options price from the market, so we can find such $\sigma$, which satisfy equallity beetwen market and model price.
Lets define $g$ as 
$$g(\sigma) = C^{\rm mkt} - f(S_t, K, r, d, \sigma, \tau)$$
and using numerical methods find $\sigma$ which satisfy
$$g(\sigma) = 0$$
Obtained $\sigma$ is called implied volatility.

---

Each day, in the market there are quoted many options with different strike prices $K$ and time to maturity $\tau$.
After computng implied volatility for all avaible optios we can put our results in matrix, like below
$$
\begin{pmatrix}
     \sigma^{\rm imp}_{T_1,K_1} & \sigma^{\rm imp}_{T_1,K_2} & \cdots & \sigma^{\rm imp}_{T_1,K_N} \cr
     \sigma^{\rm imp}_{T_2,K_1}  & \sigma^{\rm imp}_{T_2,K_2} & \cdots & \sigma^{\rm imp}_{T_2,K_N} \cr
     \vdots & \vdots & \ddots & \vdots \cr
     \sigma^{\rm imp}_{T,K_1} & \sigma^{\rm imp}_{T,K_2} & \cdots & \sigma^{\rm imp}_{T,K_N} \cr
\end{pmatrix}
$$

---

To obtain regular volatility surface we have to use interpolation  between nodes, extrapolation outside avaible nodes and same smoothing techniques.

Between each time periods $T_i < T < T_{i+1}$ (fixed $K$) linear interpolation in terms of not annualized variance is used

$$
\sigma(T) = \frac{1}{\sqrt{T}} \sqrt{\sigma^2_i T_i + \tau\left( \sigma^2_{i+1} T_{i+1} - \sigma^2_{i} T_{i} \right)}
$$
where $\tau = \frac{T - T_i}{T_{i+1} - T_i}$.

---

Interpolating between strike prices (fixed $\tau$) is a bit more tricky. We have to use more sophisticated methods like:

- polynomial regression `stats::poly()`
- local polynomial regression (loess) `stats::loess()`
- natural cubic splines `splines::ns()`
- smoothing splines `stats::smooth.spline()`
- B-spline `splines::bs()`

