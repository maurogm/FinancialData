test_that("Homogenize prices works as expected", {
  colnames <- c("lastPrice", "openPrice", "maxPrice", "minPrice", "currency", "rate")
  currencies <- c("ARS", "ARS", "CCL", "USD")
  rates <- c(10, NA, 2, NA)
  prices <- c(20, 30, 40, 10)
  df_input <- data.table(rbind(prices, prices, prices, prices)) %>%
    cbind(currencies, rates) %>%
    setnames(colnames)

  df_expected <- data.table(rbind(prices/10, NA, prices, prices)) %>%
    cbind(currencies, c(1, NA, 2, NA)) %>%
    setnames(colnames)


  expect_equal(homogenize_prices(df_input), df_expected)
})
