#' Data Loader
#'
#' Loads multiple .csv based on a regular expression and concatenates them into one data.frame
#' @param folder Path to folder where text files exist
#' @param expr Regular expression used to grab text files
#' @param ... Other arguments passed to read_csv
#' @note Uses reader and dplyr
#' @export

dataloader <- function(folder, expr, ...) {

  # Find files
  file.list <- list.files(folder, expr, full.names = TRUE)

  # Apply Reader to file.list
  dplyr::bind_rows(lapply(file.list, readr::read_csv, ...))
}
