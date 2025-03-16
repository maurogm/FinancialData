#' @title Get TCRM Data from a URL
#' @description Downloads an Excel file from the provided URL, processes the data by skipping the first row, converts the first column to Date type, and returns the data as a data.table.
#' @param local_dir A character string specifying the local directory to save the downloaded file. Defaults to "/tmp".
#' @return A data.table containing the processed TCRM data.
#' @importFrom curl new_handle curl_download
#' @importFrom readxl read_excel
#' @importFrom data.table as.data.table setnames
#' @export
#' @examples
#' \dontrun{
#'   tcrm_data <- get_tcrm()
#' }
get_tcrm <- function(local_dir = "/tmp") {
  url <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx"

  # Create a local file path based on the URL's basename.
  local_file <- file.path(local_dir, basename(url))
  
  # Create a new curl handle with SSL verification disabled.
  h <- curl::new_handle(ssl_verifypeer = 0L)
  
  # Download the file using the curl handle.
  curl::curl_download(url, destfile = local_file, handle = h)
  
  # Read the Excel file, skipping the first row.
  # The first row after skipping is assumed to be the header.
  df <- readxl::read_excel(local_file, skip = 1, col_names = TRUE)
  
  # Convert the first column to Date type using the "m/d/Y" format.
  df[[1]] <- as.Date(df[[1]], format = "%m/%d/%Y")
  
  # Identify the last row of the table by finding the last non-NA value in the first column.
  last_row <- max(which(!is.na(df[[1]])))
  
  # Subset the data frame to only include rows up to the identified last row.
  df <- df[1:last_row, ]

  # Convert the data frame to a data.table.
  dt <- data.table::as.data.table(df)

  # better column names
  data.table::setnames(dt, old = names(dt)[1], new = "date") # Rename the first column to "date".  
  names(dt) <- tolower(names(dt)) # all names to lowercase
  names(dt) <- gsub(" ", "_", names(dt)) # replace spaces with underscores.
  
  # Add ccl tcrm to the data.table
  dt <- add_ccl_tcrm(dt, 'itcrm')

  return(dt)
}


#' @title Add CCL TCRM to TCRM Data
#' @description Merges a data.table with TX Gap data and adds a new column that multiplies the specified tcrm column by the tx_gap.
#' @param dt A data.table containing TCRM data. It must have a "date" column and a column matching tcrm_column.
#' @param tcrm_column A character string specifying the name of the column in dt to be multiplied by tx_gap.
#' @return The original data.table dt with an additional column named <tcrm_column>_ccl.
#' @export
#' @examples
#' \dontrun{
#'   # Suppose dt has a "date" column and a "value" column:
#'   dt <- data.table(date = as.Date('2020-01-01') + 0:4, value = 1:5)
#'   dt <- add_ccl_tcrm(dt, "value")
#' }
add_ccl_tcrm <- function(dt, tcrm_column) {
  tx_gap <- get_tx_gap()

  new_column_name <- paste0(tcrm_column, "_ccl")

  result <- merge(dt, tx_gap[, .(date, tx_gap)], by = "date", all.x = TRUE) %>% 
    .[, (new_column_name) := get(tcrm_column) * tx_gap] %>% 
    .[, tx_gap := NULL]
  
  return(result)
}
