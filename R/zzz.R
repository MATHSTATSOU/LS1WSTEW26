.onAttach <- function(libname, pkgname) {

  if (!reticulate::py_module_available("pandas")) {
    packageStartupMessage(
      "Optional dependency 'pandas' is not installed.\n",
      "Functions depending on it (e.g., convertPy) will not work.\n",
      "Install with: reticulate::py_install('pandas')"
    )
  }
}
