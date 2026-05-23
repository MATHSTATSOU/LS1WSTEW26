#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void convertcpp(std::string dir_path) {

  Function list_files("list.files");
  Function grepl("grepl");
  Function sub("sub");

  Environment readxl = Environment::namespace_env("readxl");
  Function read_excel = readxl["read_excel"];

  Environment readr = Environment::namespace_env("readr");
  Function write_csv = readr["write_csv"];

  CharacterVector files = list_files(
    _["path"] = dir_path,
    _["full.names"] = true
  );

  for (int i = 0; i < files.size(); i++) {

    std::string file = as<std::string>(files[i]);

    bool is_excel = as<bool>(
      grepl("\\.(xlsx|xls)$", file, _["ignore.case"] = true)
    );

    if (is_excel) {
      Rcout << "Processing: " << file << std::endl;

      try {
        DataFrame df = read_excel(file);

        std::string out_file = as<std::string>(
          sub("\\.(xlsx|xls)$", ".csv", file,
              _["ignore.case"] = true)
        );

        write_csv(df, out_file);

      } catch (...) {
        Rcout << "Failed: " << file << std::endl;
      }
    }
  }
}
