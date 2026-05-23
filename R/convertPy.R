#' Convert Excel Files to CSV (Python via reticulate)
#'
#' Uses Python pandas to convert `.xls` and `.xlsx` files.
#'
#' @param dir_path Character. Directory containing files.
#'
#' @return Invisibly returns TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' convertPy("data/")
#' }
convertPy <- function(dir_path) {
  reticulate::py_run_string("
import os
import pandas as pd

def convertPy(dir_path):
    for file in os.listdir(dir_path):
        if file.lower().endswith(('.xlsx', '.xls')):
            full_path = os.path.join(dir_path, file)
            print(f'Processing: {full_path}')
            try:
                df = pd.read_excel(full_path)
                out_file = os.path.splitext(full_path)[0] + '.csv'
                df.to_csv(out_file, index=False)
            except Exception as e:
                print(f'Failed: {full_path} -> {e}')
")

  reticulate::py$convertPy(dir_path)
  invisible(TRUE)
}
