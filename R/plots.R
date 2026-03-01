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
