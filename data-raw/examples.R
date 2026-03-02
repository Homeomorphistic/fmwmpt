examples <- tidyquant::tq_get(
  c("CDR.WA", "ALE.WA", "PKO.WA",
    "NVDA", "AMZN", "TSLA", "^SP500TR",
    "BTC-USD", "GC=F",
    "AGGU.L", "VWRA.L"),
  from = as.Date("2020-01-01"),
  to   = as.Date("2026-03-01")
)

usethis::use_data(examples, overwrite = TRUE)
