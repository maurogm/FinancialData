#' @title Calculate inflation from CPI DataFrame
#' @description Takes a data.table with monthly CPI data and calculates monthly and annual inflation.
#' Also adds a column with the inflation adjustment coefficient, which is the ratio between the last CPI and the current CPI.
#' @param cpi_dt A data.table with monthly CPI data. Must have a `date` and a `cpi` column.
#' @return The same data table as the input, with the following columns added:
#' - `monthly_inflation`: the monthly inflation for that month.
#' - `annual_inflation`: the yearly inflation between that month and the same month of the previous year.
#' - `inflation_ajust_coef`: a coefficient to multiply prices of that month to adjust them for inflation
#' and translate them to current prices.
#' @importFrom stats lag
#' @export
#' @example
#' cpi_dt <- get_cpi_us()
#' calc_inflation_from_cpi(cpi_dt)
calc_inflation_from_cpi <- function(cpi_dt) {
  cpi <-monthly_inflation <- annual_inflation <- inflation_ajust_coef <- NULL # due to NSE notes in R CMD check
  dt_copy <- copy(cpi_dt[order(date)])
  dt_copy[, monthly_inflation := cpi / shift(cpi)]
  dt_copy[, annual_inflation := cpi / shift(cpi, 12)]
  dt_copy[, inflation_ajust_coef := last(cpi) / cpi]
  dt_copy
}
