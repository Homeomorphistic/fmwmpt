available_date_range <- function() {
  dates <- examples |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::pull(date) |>
    as.Date()


    return(c(tail(dates, 1), head(dates, 1)))
}

available_tickers <- function() {
  examples |>
    dplyr::distinct(symbol) |>
    dplyr::pull(symbol)
}
