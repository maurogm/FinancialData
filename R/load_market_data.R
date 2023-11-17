#' @title Load historic market data persisted by broker-management
#' @description Loads a file with historic market data from the market directory in the broker-management project.
#' @param ticker A string with the ticker of the asset to be loaded.
#' @param exchange A string identifying the stock exchange where the asset is traded.
#' @return A data frame with historic market data.
#' @importFrom readr read_csv
#' @export
#' @example
#' load_market_data("YPFD", "BCBA")
#' load_market_data("YPF", "NYSE")
#' load_market_data("VWRA", "LON")
load_market_data <- function(
    ticker,
    exchange = c("BCBA", "NYSE", "LON", "NASDAQ")) {
  path_market <- path_factory("market")
  path_exchange <- paste0(path_market, exchange)
  path_file <- paste0(path_exchange, "/", ticker, ".csv")
  if (!dir.exists(path_exchange)) {
    stop("No directory found for the Exchange '", exchange, "'.")
  } else {
    colnames <- c(
      "datetime",
      "currency",
      "lastPrice",
      "openPrice",
      "maxPrice",
      "minPrice",
      "montoOperado",
      "volumenNominal",
      "cantidadOperaciones"
    )
    data <- readr::read_csv(path_file, col_names = colnames, show_col_types = FALSE)
    # readr::read_csv because has better out-of-the-box parsing of datetime than fread
    setDT(data)
  }
}
