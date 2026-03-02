plot_prices <- function(ticker, from, to) {
  examples |>
    dplyr::filter(symbol == ticker) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(as.Date(from) <= date, date <= as.Date(to)) |>
    dplyr::select(date, close) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = close)) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}

plot_prices_with_sim <- function(ticker, from, to) {
  stock <- simulate_stock(ticker, from, to)

  stock |>
    dplyr::select(date, adjusted, simulated) |>
    tidyr::pivot_longer(-date, names_to = "type", values_to = "price") |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = price, colour = type)) +
    ggplot2::geom_line()
}

plot_normal_densities <- function(mu, sigma) {
  ggplot2::ggplot() +
    ggplot2::stat_function(fun = dnorm, args = list(mean = 0, sd = 1), ggplot2::aes(color = "N(0,1)")) +
    ggplot2::stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                           ggplot2::aes(color = sprintf("N(%g, %g)", mu, sigma))) +
    ggplot2::xlim(-4 * sigma, 4 * sigma) +
    ggplot2::labs(color = "distribution", x = "x", y = "density") +
    ggplot2::theme(legend.position = "top")
}

plot_correlated_normals <- function(rho) {
  generate_correlated_normals(1000, rho = rho) |>
    ggplot2::ggplot(ggplot2::aes(x = x1, y = x2)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth(method = "lm", color = "black")
}

plot_attainable_set <- function(mu_1, mu_2, sigma_1, sigma_2, rho) {
  attainable_with_short <- attainable_set(mu_1, mu_2, sigma_1, sigma_2, rho)
  attainable_without_short <- attainable_set(mu_1, mu_2, sigma_1, sigma_2, rho, short_selling = FALSE)

  ggplot2::ggplot(data = attainable_with_short, ggplot2::aes(x = sigma, y = mu)) +
    ggplot2::geom_path(linetype = "dashed") +
    ggplot2::geom_path(data = attainable_without_short) +

    ggplot2::labs(title = "Zbiór osiągalny", x = expression(sigma), y = expression(mu)) +
    ggplot2::xlim(0, 1.1*max(sigma_1, sigma_2)) +
    ggplot2::ylim(0, 1.1*max(mu_1, mu_2)) +

    ggplot2::geom_point(ggplot2::aes(x = sigma_1, y = mu_1)) +
    ggplot2::geom_text(ggplot2::aes(x = sigma_1, y = mu_1, label = "paste('(', mu[1], ',', sigma[1], ')')"),
                       parse = TRUE, nudge_x = -.005, nudge_y = -.005) +

    ggplot2::geom_point(ggplot2::aes(x = sigma_2, y = mu_2)) +
    ggplot2::geom_text(ggplot2::aes(x = sigma_2, y = mu_2, label = "paste('(', mu[2], ',', sigma[2], ')')"),
                       parse = TRUE, nudge_x = -.005, nudge_y = .005) +

    ggplot2::theme_classic()
}

plot_min_var <- function(mu_1, mu_2, sigma_1, sigma_2, rho) {
  mvp_weights <- minimal_variance_portfolio(sigma_1, sigma_2, rho)

  mvp_mu <- sum(c(mu_1, mu_2) * mvp_weights)
  mvp_sigma <- portfolio_volatility(sigma_1, sigma_2, rho, weights = mvp_weights)

  plot_attainable_set(mu_1, mu_2, sigma_1, sigma_2, rho) +
    ggplot2::geom_point(ggplot2::aes(x = mvp_sigma, y = mvp_mu)) +
    ggplot2::geom_text(ggplot2::aes(x = mvp_sigma, y = mvp_mu, label = "MVP"),
                       nudge_x = -.005, nudge_y = .005)
}

plot_market_portfolio <- function(mu_1, mu_2, sigma_1, sigma_2, rho, risk_free) {
  mp_weights <- market_portfolio(mu_1, mu_2, sigma_1, sigma_2, rho, risk_free)

  mp_mu <- sum(c(mu_1, mu_2) * mp_weights)
  mp_sigma <- portfolio_volatility(sigma_1, sigma_2, rho, weights = mp_weights)

  cml <- function(sigma) {
    risk_free + (mp_mu-risk_free) / mp_sigma * sigma
  }
  sigma_cml <- seq(0, max(sigma_1, sigma_2), length.out = 100)
  cml_data <- data.frame(sigma_c = sigma_cml, mu_c = cml(sigma_cml))

  plot_min_var(mu_1, mu_2, sigma_1, sigma_2, rho) +
    ggplot2::geom_point(ggplot2::aes(x = mp_sigma, y = mp_mu)) +
    ggplot2::geom_text(ggplot2::aes(x = mp_sigma, y = mp_mu, label = "MP"),
                       nudge_x = -.005, nudge_y = .005) +
    ggplot2::geom_point(ggplot2::aes(x = 0, y = risk_free)) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = risk_free, label = "R"),
                       nudge_x = .005, nudge_y = .005) +
    ggplot2::geom_path(data = cml_data, ggplot2::aes(x = sigma_c, y = mu_c))
}

