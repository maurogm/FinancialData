test_that("monthly inflation is as expected", {
  example_dt <- data.table::data.table(
    date = as.Date(c("2010-01-01", "2010-02-01", "2010-03-01", "2010-04-01")),
    cpi = c(100, 110, 121, 133.1)
  )
  inflation_dt <- calc_inflation_from_cpi(example_dt)

  expect_equal(inflation_dt$monthly_inflation, c(NA, 1.1, 1.1, 1.1))
})
