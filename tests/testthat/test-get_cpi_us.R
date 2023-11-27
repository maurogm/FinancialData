cpi_dt <- get_cpi_us()
  
test_that("the CPI for Jan 2010 is correct", {
  expect_equal(cpi_dt[date == "2010-01-01", cpi], 216.687, tolerance = 0.001)
})

test_that("there are no missing dates", {
  min_date <- min(cpi_dt$date)
  max_date <- max(cpi_dt$date)
  all_dates <- seq.Date(min_date, max_date, by = "month")
  expect_equal(length(all_dates), nrow(cpi_dt))
})
