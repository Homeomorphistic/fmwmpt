examples <- tidyquant::tq_get(
  c("CDR.WA", "TSLA", "BTC-USD", "GC=F", "AGGU.L", "VWRA.L"),
  from = Sys.Date() - 365 * 2,
  to   = Sys.Date()
)

usethis::use_data(examples, overwrite = TRUE)
