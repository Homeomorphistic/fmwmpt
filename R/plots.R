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
