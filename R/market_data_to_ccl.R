#' @title Convert market data prices to CCL
#' @description Use this function to convert a data.table with market data to CCL currency.
#' If there is no exchange rate for a certain date, the original price is lost.
#' @param df_market A data.table with the market data, as returned by `load_market_data()`
#' @param df_exch_rate A data.table with the exchange rate data, as returned by `load_ccl()
#' @import magrittr
#' @import data.table
#' @importFrom dplyr select all_of
#' @return The same data.table with it's prices converted to the new currency.
#' @export
#' @example
#' df_market <- load_market_data("YPFD", "BCBA")
#' df_exch_rate <- load_ccl("cclGGAL")
#' market_data_to_ccl(df_market, df_exch_rate)
market_data_to_ccl <- function(df_market, df_exch_rate) {
    datetime <- . <- NULL # due to NSE notes in R CMD check
    original_columns <- copy(colnames(df_market))
    df_market %>%
        .[, date := as.Date(datetime)] %>%
        merge(df_exch_rate, by.x = c("date", "currency"), by.y = c("date", "currency_from"), all.x = TRUE) %>%
        homogenize_prices() %>%
        setnames(c("currency", "currency_to"), c("currency_from", "currency")) %>%
        dplyr::select(dplyr::all_of(original_columns)) %>%
        setDT()
}


needs_currency_conversion <- function(df) {
    # returns FALSE if currency contains "USD" or "CCL" strings:
    !grepl("USD|CCL", df$currency)
}


#' @title Transform the price columns of a data.table by applying the exchange rate
#' @description Use this function to convert the currencies of a data.table with market data
#' that already includes the exchange rate for each date.
#' Modifies the original data.table in place.
#' @param dt A data.table with the following columns: currency, lastPrice, openPrice, maxPrice, minPrice, rate
#' @import data.table
#' @return The same data.table with the price columns transformed.
homogenize_prices <- function(dt) {
    rate <- NULL # due to NSE notes in R CMD check
    index_to_convert <- needs_currency_conversion(dt)
    prices <- c("lastPrice", "openPrice", "maxPrice", "minPrice")
    homogenize_price <- function(price) dt[index_to_convert, (price) := get(price) / rate]
    lapply(prices, homogenize_price)
    dt[index_to_convert & !is.na(rate), rate := 1]
}
