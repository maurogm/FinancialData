#' @title Get TX A3500 Data from a URL
#' @description Downloads an Excel file from the provided URL, reads data from headers on row 4, extracts only the second and third columns, converts the first extracted column to Date type (using format "yy-mm-dd"), renames the columns to "date" and "tc_a3500", and returns the data as a data.table.
#' @param local_dir A character string specifying the local directory to save the downloaded file. Defaults to "/tmp".
#' @return A data.table containing the processed TX A3500 data.
#' @importFrom curl new_handle curl_download
#' @importFrom readxl read_excel
#' @importFrom data.table as.data.table setnames
#' @export
#' @examples
#' \dontrun{
#'   tc_a3500_data <- get_tc_a3500()
#' }
get_tx_a3500 <- function(local_dir = "/tmp") {
  url <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/com3500.xls"
  
  # Create a local file path based on the URL's basename.
  local_file <- file.path(local_dir, basename(url))
  
  # Create a new curl handle with SSL verification disabled.
  h <- curl::new_handle(ssl_verifypeer = 0L)
  
  # Download the file using the curl handle.
  curl::curl_download(url, destfile = local_file, handle = h)
  
  # Read the Excel file. Skip the first 3 rows so that the header is on row 4.
  df <- readxl::read_excel(local_file, skip = 3, col_names = TRUE, col_types = c("date", "numeric", "skip", "skip"))
  
  # Convert the first column to Date type using the "yy-mm-dd" format.
  df[[1]] <- as.Date(df[[1]], format = "%y-%m-%d")
  
  # Convert the data frame to a data.table.
  dt <- data.table::as.data.table(df)
  
  # Rename columns: the first column as "date" and the second as "tc_a3500".
  data.table::setnames(dt, old = names(dt), new = c("date", "tc_a3500"))
  
  # Return the processed data.table.
  return(dt)
}
