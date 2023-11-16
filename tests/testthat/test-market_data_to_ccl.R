test_that("toy example of market data price conversion works as expected", {
  df_market <- data.table(
    datetime = c("2001-12-07T17:05", "2001-12-08T00:00") %>% lubridate::ymd_hm(),
    currency = c("ARS", "ARS"),
    lastPrice = c(10, 20),
    openPrice = c(10, 20),
    maxPrice = c(10, 20),
    minPrice = c(10, 20)
  )
  df_exch_rate <- data.table(
    currency_from = c("ARS", "ARS"),
    currency_to = c("USD-CCL", "USD-CCL"),
    date = c("2001-12-07", "2001-12-08") %>% lubridate::ymd(),
    rate = c(2, 10)
  )
  df_expected <- data.table(
    datetime = c("2001-12-07T17:05", "2001-12-08T00:00") %>% lubridate::ymd_hm(),
    currency = c("USD-CCL", "USD-CCL"),
    lastPrice = c(5, 2),
    openPrice = c(5, 2),
    maxPrice = c(5, 2),
    minPrice = c(5, 2)
  )

  expect_equal(
    market_data_to_ccl(df_market, df_exch_rate),
    df_expected,
    ignore_attr = TRUE
  )
})
