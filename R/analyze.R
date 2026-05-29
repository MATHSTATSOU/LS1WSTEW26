#' Analyze a Directory
#'
#' Collect file metadata, summaries, and visualization objects.
#'
#' @param path Directory path
#' @param top_n Number of top file types
#' @param hash Whether to compute file hashes
#'
#' @return A list containing file data, summary, plot, table, and duplicates
#' @export
analyze_dir <- function(path, top_n = 10, hash = FALSE) {

  files <- fs::dir_info(path, recurse = FALSE)

  if (nrow(files) == 0) return(NULL)

  df <- data.frame(
    name = base::basename(files$path),
    path = files$path,
    type = files$type,
    size_mb = as.numeric(files$size)/(1024^2),
    stringsAsFactors = FALSE
  )

  df$Extension <- toupper(tools::file_ext(df$name))
  df$Extension[df$Extension == ""] <- "NO EXT"

  df$MIME <- mime::guess_type(df$path)

  if (hash) {
    df$hash <- NA
    file_rows <- df$type == "file"

    df$hash[file_rows] <- vapply(
      df$path[file_rows],
      digest::digest,
      "",
      file = TRUE
    )
  }

  summary_df <- stats::aggregate(
    size_mb ~ Extension,
    df[df$type == "file", ],
    function(x) c(Count = length(x), Total = sum(x))
  )

  summary_df <- data.frame(
    Extension = summary_df$Extension,
    Count = summary_df$size_mb[,1],
    Total_MB = summary_df$size_mb[,2]
  )

  summary_df <- summary_df[order(-summary_df$Count), ]
  top_df <- utils::head(summary_df, top_n)

  plot <- ggplot2::ggplot(
    df[df$type == "file", ],
    ggplot2::aes(
      x = Extension,
      y = size_mb,
      text = paste(name, "<br>", round(size_mb,2), "MB")
    )
  ) +
    ggplot2::geom_boxplot(fill = "orange") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  table <- gt::gt(top_df) |>
    gt::tab_header(title = "Top File Types") |>
    gt::fmt_number(columns = Total_MB, decimals = 2) |>
    gt::data_color(
      columns = Count,
      fn = scales::col_numeric(c("#e8f5e9","#2e7d32"), NULL)
    )

  dup <- NULL
  if (hash) {
    dup <- df[duplicated(df$hash) & !is.na(df$hash), ]
  }

  list(df = df, summary = summary_df, plot = plot, table = table, dup = dup)
}
