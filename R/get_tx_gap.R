#' @title Get TX Gap Data
#' @description Loads CCL data using `load_ccl()` and TC A3500 data using `get_tc_a3500()`, then performs a rolling join on the "date" column. The CCL data is subset to include only the date and the rate (renamed as "ccl") before joining.
#' @return A data.table containing the joined data with TC A3500 and CCL data.
#' @export
#' @examples
#' \dontrun{
#'   tx_gap <- get_tx_gap()
#' }
get_tx_gap <- function() {
  # Load CCL data
  ccl <- load_ccl()
  
  # Load TC A3500 data
  a3500 <- get_tx_a3500()
  
  # Perform a rolling join on "date" before adding the gap column.
  result <- a3500[ccl[, .(date, ccl = rate)], on = "date", roll = TRUE] %>% 
    .[, tx_gap := ccl / tc_a3500]

  return(result)
}
