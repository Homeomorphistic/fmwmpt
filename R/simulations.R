simulate_stock <- function(ticker, from, to) {
  stock <- examples |>
    dplyr::filter(symbol == ticker) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(as.Date(from) <= date, date <= as.Date(to))

  n_days <- nrow(stock)
  S0 <- stock$adjusted[1]

  returns <- stock |>
    tidyquant::tq_transmute(select = adjusted,
                            mutate_fun = periodReturn,
                            period = "daily",
                            type = "log",
                            col_rename = "log_return")

  mu    <- mean(returns$log_return) * 252
  sigma <- sd(returns$log_return) * sqrt(252)

  stock |>
    dplyr::mutate(simulated = simulate_gbm(S0, mu, sigma, n_days, n_sims = 1)[,1])
}

simulate_gbm <- function(S0, mu, sigma, n_days, n_sims = 100, dt = 1/252) {
  # Matrix of random shocks
  Z <- matrix(rnorm(n_days-1 * n_sims), nrow = n_days-1, ncol = n_sims)

  # Daily log returns:
  log_returns <- (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z

  # Cumulative product to get price paths
  price_paths <- apply(log_returns, 2, function(r) S0 * cumprod(exp(r)))

  rbind(S0, price_paths)
}

generate_correlated_normals <- function(n, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho) {
  z1 <- rnorm(n)
  z2 <- rnorm(n)

  x1 <- mu1 + sigma1 * z1
  x2 <- mu2 + sigma2 * (rho * z1 + sqrt(1 - rho^2) * z2)

  data.frame(x1, x2)
}
