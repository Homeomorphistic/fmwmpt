last_price <- function(symbol = "PKO.WA") {
  prices <- quantmod::getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
  as.numeric(quantmod::Cl(xts::last(prices)))

  return(rnorm(1))
}
prices <- quantmod::getSymbols("PKO.WA", src = "yahoo", auto.assign = FALSE)

tidyquant::tq_get(c("PKO.WA", "AAPL"), get = "stock.prices") |>
  tidyquant::tq_transmute(select = close, mutate_fun = monthlyReturn)
