tcrm_dt <- get_tcrm()

test_that("the ITCRM for the base date is 100", {
  expect_equal(tcrm_dt[date == "2015-12-17", itcrm], 100, tolerance = 0.001)
})

test_that("the ITCRM for 2025-03-10 is correct", {
  expect_equal(tcrm_dt[date == "2025-03-10", itcrm], 80.9, tolerance = 0.001)
})

test_that("there are no missing dates", {
  min_date <- min(tcrm_dt$date)
  max_date <- max(tcrm_dt$date)
  all_dates <- seq.Date(min_date, max_date, by = "day")
  expect_equal(length(all_dates), nrow(tcrm_dt))
})
