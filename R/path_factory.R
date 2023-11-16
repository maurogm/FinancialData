# Documentation of package function:
#
#' @title Get path to a folder in the broker-management project
#' @description Factory method for getting a string with the path to a folder in the broker-management project
#' @param dir_id A string identifying the directory to get the path to.
#' Can be one of "resources", "outputs", "currencies", "market".
#' @return A string with the path to the folder
#' @export
#' @examples
#' path_factory("resources")
#' path_factory("outputs")
#' path_factory("currencies")
#' path_factory("market")
path_factory <- function(
    dir_id = c("resources", "outputs", "currencies", "market")) {
  path_resources <- "~/Proyectos/Personales/broker-management/src/main/resources/"
  path_outputs <- paste0(path_resources, "outputs/")
  path_currencies <- paste0(path_outputs, "currencies/")
  path_market <- paste0(path_resources, "market/")
  switch(dir_id,
    "resources" = path_resources,
    "outputs" = path_outputs,
    "currencies" = path_currencies,
    "market" = path_market
  )
}
