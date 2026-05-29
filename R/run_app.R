#' Run the Directory Explorer App
#'
#' Launch the Shiny app with optional arguments.
#'
#' @param dir_path Starting directory (default = home directory)
#' @param top_n Default number of top file types
#' @param hash Whether duplicate detection is enabled by default
#'
#' @export
run_app <- function(dir_path = fs::path_home(),
                    top_n = 10,
                    hash = FALSE) {

  app_dir <- system.file("app", package = "LS1WSTEW26")

  if (app_dir == "") {
    stop("Cannot find app directory")
  }

  # ✅ Pass arguments via options
  options(
    dirspect.dir_path = dir_path,
    dirspect.top_n = top_n,
    dirspect.hash = hash
  )

  shiny::runApp(app_dir, display.mode = "normal")
}
