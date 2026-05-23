#' Convert Excel Files to CSV (R version)
#'
#' Lists files in a directory and converts `.xls` and `.xlsx` files to `.csv`.
#'
#' @param dir_path Character. Directory containing files.
#'
#' @return Invisibly returns TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' convertR("data/")
#' }
convertR <- function(dir_path) {
  files <- list.files(dir_path, full.names = TRUE)

  for (file in files) {
    if (grepl("\\.(xlsx|xls)$", file, ignore.case = TRUE)) {
      message("Processing: ", file)

      df <- tryCatch(
        readxl::read_excel(file),
        error = function(e) {
          warning("Failed: ", file)
          NULL
        }
      )

      if (!is.null(df)) {
        out_file <- sub("\\.(xlsx|xls)$", ".csv", file, ignore.case = TRUE)
        readr::write_csv(df, out_file)
      }
    }
  }

  invisible(TRUE)
}
