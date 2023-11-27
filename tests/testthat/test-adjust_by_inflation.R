example_dt <- data.table::data.table(
  my_date = as.Date(c("2010-01-01", "2011-05-02", "2012-06-03", "2013-12-04", "2014-08-05", "2015-09-06")),
  price1 = rep(10, 6),
  price2 = c(100, 200, 300, 400, 500, 600)
)
example_inflation_dt <- data.table::data.table(
  date = as.Date(c("2011-05-01", "2012-06-01", "2013-07-01", "2014-08-01")),
  inflation_ajust_coef = c(3, 2, 2, 1.5)
)

test_that("after adjustment there are 2 new columns, plus the original ones", {
  adjusted_dt <- adjust_by_inflation(example_dt, example_inflation_dt, date_column = "my_date", price_columns = c("price1", "price2"))
  expect_named(adjusted_dt,
    c("my_date", "price1", "price2", "price1_adjusted", "price2_adjusted"),
    ignore.order = TRUE
  )
})

test_that("the adjusted values for data after the last inflation value are the unadjusted values", {
  adjusted_dt <- adjust_by_inflation(example_dt, example_inflation_dt, date_column = "my_date", price_columns = "price1")
  expect_equal(adjusted_dt[my_date >= "2014-09-01", price1_adjusted], adjusted_dt[my_date >= "2014-09-01", price1])
})

test_that("the adjusted value when there is no inflation value fur dates prior to the last inflation value is NA", {
  adjusted_dt <- adjust_by_inflation(example_dt, example_inflation_dt, date_column = "my_date", price_columns = "price1")
  expect_true(is.na(adjusted_dt[my_date == "2010-01-01", price1_adjusted]))
  expect_true(is.na(adjusted_dt[my_date == "2013-12-04", price1_adjusted]))
})

test_that("the values for price1 are adjusted correctly", {
  adjusted_dt <- adjust_by_inflation(example_dt, example_inflation_dt, date_column = "my_date", price_columns = "price1")
  expect_equal(adjusted_dt[my_date == "2011-05-02", price1_adjusted], 30)
  expect_equal(adjusted_dt[my_date == "2012-06-03", price1_adjusted], 20)
  expect_equal(adjusted_dt[my_date == "2014-08-05", price1_adjusted], 15)
  expect_equal(adjusted_dt[my_date == "2015-09-06", price1_adjusted], 10)
})

test_that("the values for price2 are adjusted correctly", {
  adjusted_dt <- adjust_by_inflation(example_dt, example_inflation_dt, date_column = "my_date", price_columns = "price2")
  expect_equal(adjusted_dt[my_date == "2011-05-02", price2_adjusted], 600)
  expect_equal(adjusted_dt[my_date == "2012-06-03", price2_adjusted], 600)
  expect_equal(adjusted_dt[my_date == "2014-08-05", price2_adjusted], 750)
  expect_equal(adjusted_dt[my_date == "2015-09-06", price2_adjusted], 600)
})
