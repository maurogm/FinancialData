#' @title Load certain datasets persisted by broker-management
#' @description Factory method for loading a dataframe from the outputs directory in the broker-management project
#' @param data_id A string identifying the dataset to be loaded.
#' Can be one of "current_position", "recent_prices", "unified_movements", "unified_orders"
#' @return A data frame
#' @export
#' @examples
#' load_my_data("current_position")
#' load_my_data("recent_prices")
#' load_my_data("unified_movements")
#' load_my_data("unified_orders")
load_my_data <- function(
    data_id = c("current_position", "recent_prices", "unified_movements", "unified_orders")) {
  path_outputs <- path_factory("outputs")
  file_name <- switch(data_id,
    "current_position" = "current_position.csv",
    "recent_prices" = "recent_prices.csv",
    "unified_movements" = "unified_movements.csv",
    "unified_orders" = "unified_orders.csv",
  )
  file_path <- paste0(path_outputs, file_name)
  data.table::fread(file_path)
}
