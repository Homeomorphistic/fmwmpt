stock_last_returns <- function(ticker, period = "monthly", last=5) {
  examples |>
    dplyr::filter(symbol == ticker) |>
    tidyquant::tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = period,
      col_rename = "return"
    ) |>
    dplyr::slice_tail(n = last)
}

stocks_summary <- function(tickers, period = "monthly") {
  examples |>
    dplyr::filter(symbol %in% tickers) |>
    dplyr::group_by(symbol) |>
    tidyquant::tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = period,
      col_rename = "return"
    ) |>
    dplyr::summarise(
      avg_return  = mean(return, na.rm = TRUE),
      volatility  = sd(return, na.rm = TRUE)
    )
}
