#' @title Adjust price columns by inflation
#' @description Takes a data.table with prices and a data.table with inflation coefficients
#' (such as the one produced by the `calc_inflation_from_cpi` function) and returns a data.table
#' with the same columns as the original one, plus the adjusted price columns.
#' 
#' Prices for dates after the last inflation coefficient are not adjusted.
#' @param dt A data.table where each row contains one or more prices on a certain date.
#' @param inflation_dt A data.table a `date` column containing the first day of each month,
#' and an `inflation_ajust_coef` column with the inflation adjustment coefficient for that month,
#' as produced by the `calc_inflation_from_cpi` function.
#' @param date_column A String with the name of the date column in `dt`.
#' @param price_columns A vector of Strings with the names of the price columns in dt to be adjusted.
#' @return A data.table with the same columns as `dt`, plus the adjusted price columns.
#' @importFrom dplyr mutate select all_of
#' @importFrom lubridate floor_date
#' @import data.table
#' @export
#' @example
#' cpi_dt <- get_cpi_us()
#' inflation_dt <- calc_inflation_from_cpi(cpi_dt)
#' spy <- load_market_data("SPY", "NYSE")
#' spy_adjusted <- adjust_by_inflation(spy, inflation_dt, date_column = "datetime", price_columns = c("lastPrice", "openPrice", "maxPrice", "minPrice"))
adjust_by_inflation <- function(dt,
                                inflation_dt,
                                date_column = "date",
                                price_columns = c("lastPrice", "openPrice", "maxPrice", "minPrice")) {
    first_of_month <- inflation_ajust_coef <- NULL # due to NSE notes in R CMD check
    original_columns <- copy(colnames(dt))
    max_date_with_cpi <- max(inflation_dt$date)
    merged_dt <- dt %>%
        dplyr::mutate(first_of_month = lubridate::floor_date(get(date_column), "month") %>% as.Date()) %>%
        merge(select(inflation_dt, date, inflation_ajust_coef),
            by.y = "date", by.x = "first_of_month", all.x = TRUE
        )
    adjust_price_column <- function(price_column) {
        new_col <- paste0(price_column, "_adjusted")
        merged_dt[, (new_col) :=
            dplyr::if_else(first_of_month > max_date_with_cpi,
                get(price_column),
                get(price_column) * inflation_ajust_coef
            )]
    }
    lapply(price_columns, adjust_price_column)
    new_columns <- paste0(price_columns, "_adjusted")
    merged_dt %>%
        dplyr::select(dplyr::all_of(c(original_columns, new_columns)))
}
