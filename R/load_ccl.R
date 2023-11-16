#' @title Load historic CCL data persisted by broker-management
#' @description Loads a file with historic CCL data from the currencies directory in the broker-management project.
#' The historic values have been obtained as the ratio between the price of a certain stock and the price of it's GDR.
#' @param ccl_id A string with the filename of the CSV to be loaded.
#' @return A data frame with historic exchange rates.
#' @export
#' @example
#' load_ccl("cclGGAL")
#' load_ccl("cclAL30")
#' load_ccl("cclGD30")
load_ccl <- function(ccl_id = "cclGGAL") {
  path_currencies <- path_factory("currencies")
  file_path <- paste0(path_currencies, ccl_id, ".csv")
  data.table::fread(file_path)
}
