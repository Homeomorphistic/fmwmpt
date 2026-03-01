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
