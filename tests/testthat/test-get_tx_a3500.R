a3500_dt <- get_tx_a3500()

test_that("the ITCRM for 2025-03-10 is correct", {
  expect_equal(a3500_dt[date == "2020-07-07", tc_a3500], 70.8750, tolerance = 0.001)
})
