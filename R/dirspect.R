#' Inspect a Directory and Visualize File Types
#'
#' Analyze a directory to summarize file types, counts, sizes,
#' and generate plots and a formatted table.
#'
#' @param dir_path Character. Directory path (default = ".").
#' @param top_n Integer. Number of top file types to display (default = 10).
#'
#' @return Invisibly returns a data.frame containing file metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' dirspect(".")
#' }
dirspect <- function(dir_path = ".", top_n = 10) {

  # ---- Validate directory ----
  if (!dir.exists(dir_path)) {
    stop("The specified directory does not exist.")
  }

  # ---- Get file info ----
  file_info <- base::file.info(
    base::list.files(dir_path,
                     full.names = TRUE,
                     recursive = FALSE)
  )

  file_info <- file_info[!file_info$isdir, ]

  if (nrow(file_info) == 0) {
    message("No files found.")
    return(NULL)
  }

  # ---- Extract extensions ----
  file_names <- base::basename(rownames(file_info))
  file_exts <- tools::file_ext(file_names)

  file_exts[file_exts == ""] <- "No Extension"
  file_exts <- base::toupper(file_exts)

  # ---- Build dataset ----
  df <- base::data.frame(
    Extension = base::as.factor(file_exts),
    Size_MB = file_info$size / (1024 * 1024)
  )

  # ---- Summary ----
  summary_df <- stats::aggregate(
    Size_MB ~ Extension,
    data = df,
    FUN = function(x) c(Count = length(x), Total_Size = sum(x))
  )

  summary_df <- base::data.frame(
    Extension = summary_df$Extension,
    Count = summary_df$Size_MB[, "Count"],
    Total_Size = summary_df$Size_MB[, "Total_Size"]
  )

  summary_df <- summary_df[base::order(-summary_df$Count), ]

  # ---- Top-N + Pareto stats ----
  top_table <- utils::head(summary_df, top_n)

  total_count <- base::sum(summary_df$Count)
  total_size  <- base::sum(summary_df$Total_Size)

  top_table$Pct_Count <- top_table$Count / total_count
  top_table$Pct_Size  <- top_table$Total_Size / total_size

  top_table$Cum_Count <- base::cumsum(top_table$Count)
  top_table$Cum_Pct   <- top_table$Cum_Count / total_count

  top_table$Total_Size <- base::round(top_table$Total_Size, 2)

  # ---- GT table (CRAN-safe) ----
  gt_table <- gt::gt(top_table) |>

    gt::tab_header(
      title = gt::md("**Directory Summary Dashboard**"),
      subtitle = paste("Top", top_n, "File Types")
    ) |>

    gt::cols_label(
      Extension = "Type",
      Count = "Files",
      Total_Size = "Size (MB)",
      Pct_Count = "% Files",
      Pct_Size  = "% Size",
      Cum_Pct   = "Cum %"
    ) |>

    gt::fmt_number(columns = Total_Size, decimals = 2) |>
    gt::fmt_percent(columns = c(Pct_Count, Pct_Size, Cum_Pct), decimals = 1) |>

    # Stable color gradients (no fragile APIs)
    gt::data_color(
      columns = Count,
      fn = scales::col_numeric(
        palette = c("#e8f5e9", "#2e7d32"),
        domain = NULL
      )
    ) |>

    gt::data_color(
      columns = Total_Size,
      fn = scales::col_numeric(
        palette = c("#e3f2fd", "#1565c0"),
        domain = NULL
      )
    ) |>

    gt::data_color(
      columns = Cum_Pct,
      fn = scales::col_numeric(
        palette = c("#fff3e0", "#fb8c00"),
        domain = c(0, 1)
      )
    ) |>

    gt::cols_align("center", gt::everything()) |>

    # Highlight top 80% contributors (Pareto)
    gt::tab_style(
      style = list(gt::cell_fill(color = "#e8f5e9")),
      locations = gt::cells_body(rows = Cum_Pct <= 0.8)
    ) |>

    gt::opt_row_striping() |>

    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#263238"),
        gt::cell_text(color = "white", weight = "bold")
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>

    gt::tab_options(
      table.width = gt::pct(90),
      table.font.size = 12,
      heading.align = "center"
    )

  # ---- Plot: File counts ----
  p_bar <- ggplot2::ggplot(df, ggplot2::aes(x = Extension)) +
    ggplot2::geom_bar(fill = "steelblue", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "File Count by Extension",
      x = "Extension",
      y = "Count"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # ---- Plot: File size distribution ----
  p_box <- ggplot2::ggplot(df, ggplot2::aes(x = Extension, y = Size_MB)) +
    ggplot2::geom_boxplot(fill = "orange", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "File Size Distribution",
      x = "Extension",
      y = "Size (MB)"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # ---- Pareto (Count) ----
  pareto <- summary_df
  pareto$cum_pct <- base::cumsum(pareto$Count) / base::sum(pareto$Count)

  p_pareto <- ggplot2::ggplot(
    pareto,
    ggplot2::aes(x = stats::reorder(Extension, -Count))
  ) +
    ggplot2::geom_col(ggplot2::aes(y = Count),
                      fill = "gray70", color = "black") +
    ggplot2::geom_line(
      ggplot2::aes(y = cum_pct * max(Count), group = 1),
      color = "red",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = cum_pct * max(Count)),
      color = "red",
      size = 2
    ) +
    ggplot2::scale_y_continuous(
      name = "Count",
      sec.axis = ggplot2::sec_axis(
        ~ . / max(pareto$Count),
        name = "Cumulative %"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Pareto Chart (Count)") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # ---- Combine plots ----
  combined_plot <-
    (p_bar + p_box) / p_pareto +
    patchwork::plot_annotation(
      title = paste("Directory:", normalizePath(dir_path))
    )

  print(combined_plot)
  print(gt_table)

  invisible(df)
}
