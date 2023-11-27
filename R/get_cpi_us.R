#' @title Get monthly Consumer Price Index data from FRED
#' @description Wrapper for getSymbols from quantmod to download monthly CPI data from FRED.
#' @return A data table with monthly CPI data.
#' @importFrom quantmod getSymbols
#' @importFrom zoo index coredata
#' @export
#' @example
#' get_cpi_us()
get_cpi_us <- function() {
    # Define the CPI symbol for FRED
    cpi_symbol <- "CPIAUCNS"

    # Download CPI data using getSymbols from quantmod
    start_date <- "1900-01-01"
    end_date <- Sys.Date()
    quantmod::getSymbols(cpi_symbol, src = "FRED", from = start_date, to = end_date, auto.assign = TRUE)

    # Extract CPI data from the environment
    cpi_data <- get(cpi_symbol)

    # Create a data frame with date and CPI columns
    cpi_dt <- data.table::data.table(
        date = zoo::index(cpi_data),
        cpi = zoo::coredata(cpi_data)
    ) %>%
        data.table::setnames(c("date", "cpi"))

    # Return the data table
    cpi_dt
}
