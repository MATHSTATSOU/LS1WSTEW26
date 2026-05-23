.onLoad <- function(libname, pkgname) {
  if (reticulate::py_available(initialize = FALSE)) {
    if (!reticulate::py_module_available("pandas")) {
      message("Note: pandas not installed. convertPy() will not work until installed.")
    }
  }
}
