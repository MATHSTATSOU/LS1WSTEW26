#' Convert Excel Files to CSV (C++ via Rcpp)
#'
#' Uses Rcpp loop with R readxl/readr backend.
#'
#' @param dir_path Character. Directory containing files.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' convertcpp("data/")
#' }
convertcpp <- function(dir_path) {
    .Call(`_LS1WSTEW26_convertcpp`, dir_path)
}
