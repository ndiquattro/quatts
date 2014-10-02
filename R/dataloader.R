#' Data Loader
#'
#' Loads multiple .csv based on a regular expression and concatenates them into one data.frame
#' @param folder Path to folder where text files exist
#' @param expr Regular expression used to grab text files
#' @param cnames Character vector of column names to use in produced data.frame
#' @param ... Other arguments passed to read.csv
#' @note stringsAsFactors is hardcoded to FALSE
#' @export

dataloader <- function(folder, expr, ...) {
  # Find files
  file.list <- list.files(folder, expr, full.names = TRUE)

  # Make read.csv function
  Reader <- function(files){
    read.csv(files, stringsAsFactors = FALSE, ...)
  }

  # Apply Reader to file.list
  datf <- do.call("rbind", lapply(file.list, Reader))
}
