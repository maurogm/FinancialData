test_that("NA values return TRUE", {
  df_test <- data.table(
    currency = c(NA, NA),
    rate = c(2, NA)
  )
  expect_equal(needs_currency_conversion(df_test), c(TRUE, TRUE))
})

test_that("USD values return FALSE", {
  df_test <- data.table(
    currency = c("USD", "USD currency", "my USD currency"),
    rate = c(2, 3, 4)
  )
  expect_equal(needs_currency_conversion(df_test), c(FALSE, FALSE, FALSE))
})

test_that("CCL values return FALSE", {
  df_test <- data.table(
    currency = c("CCL", "CCL currency", "my CCL currency"),
    rate = c(2, 3, 4)
  )
  expect_equal(needs_currency_conversion(df_test), c(FALSE, FALSE, FALSE))
})

test_that("Non-USD currencies return TRUE", {
  df_test <- data.table(
    currency = c("ARS", "MXN", "EUR"),
    rate = c(2, 3, 4)
  )
  expect_equal(needs_currency_conversion(df_test), c(TRUE, TRUE, TRUE))
})

test_that("A mixed example works fine", {
  df_test <- data.table(
    currency = c("USD", "CCL", "USD-CCL", "ARS", NA),
    rate = c(1000, NA, -1, 2, NA)
  )
  expect_equal(needs_currency_conversion(df_test), c(FALSE, FALSE, FALSE, TRUE, TRUE))
})
