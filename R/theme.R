#' Define a clean, journal-style figure theme
#'
#' Produces a black/white/gray ggplot2 theme suitable for academic
#' manuscripts (no colored backgrounds, minimal gridlines, serif or sans
#' options, print-safe). Used by every `plot_*` and `generate_all_figures()`
#' call in this package. Colors are used only functionally (e.g. significant
#' vs non-significant paths), never decoratively.
#'
#' @param base_family Font family. Default "" (device default); set to
#'   "serif" for a Times-like look common in journals, or "sans" for a
#'   Helvetica-like look.
#' @param base_size Base font size in points.
#' @param grid Show faint gridlines? Default TRUE (light gray, minor
#'   gridlines suppressed).
#' @return An object of class `plssem_theme`: a list containing a ggplot2
#'   `theme()` object (`$gg`) and a small palette (`$palette`) used by the
#'   non-ggplot (SVG path-diagram) figures for consistency.
#' @export
plssem_theme <- function(base_family = "", base_size = 11, grid = TRUE) {
  gg <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0),
      plot.subtitle = ggplot2::element_text(color = "gray30", size = ggplot2::rel(0.9), hjust = 0),
      plot.caption = ggplot2::element_text(color = "gray40", size = ggplot2::rel(0.75), hjust = 0),
      axis.title = ggplot2::element_text(face = "plain", color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = if (grid) {
        ggplot2::element_line(color = "gray88", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.border = ggplot2::element_rect(color = "gray70", fill = NA, linewidth = 0.4),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "gray95", color = NA),
      strip.text = ggplot2::element_text(face = "bold", color = "black"),
      plot.margin = ggplot2::margin(12, 14, 10, 10)
    )

  palette <- list(
    significant = "#1a1a1a",
    nonsignificant = "#b0b0b0",
    positive = "#1a1a1a",
    negative = "#8c8c8c",
    fill_low = "#f0f0f0",
    fill_high = "#4d4d4d",
    accent = "#3b3b3b"
  )

  structure(list(gg = gg, palette = palette, base_family = base_family,
                 base_size = base_size), class = "plssem_theme")
}

#' Save a ggplot figure in the requested formats
#'
#' Internal helper used by all `plot_*` functions to consistently export
#' PNG (300 dpi, for Word/PowerPoint) and PDF (vector, for LaTeX) versions.
#'
#' @param plot A ggplot object.
#' @param path_no_ext Output path without file extension.
#' @param width,height Figure size in inches.
#' @param formats Character vector, subset of c("png", "pdf").
#' @param dpi Resolution for raster formats.
#' @return Invisibly, a named character vector of paths written.
#' @keywords internal
#' @export
save_figure <- function(plot, path_no_ext, width = 7, height = 5,
                         formats = c("png", "pdf"), dpi = 300) {
  paths <- character(0)
  if ("png" %in% formats) {
    p <- paste0(path_no_ext, ".png")
    ggplot2::ggsave(p, plot, width = width, height = height, dpi = dpi, bg = "white")
    paths["png"] <- p
  }
  if ("pdf" %in% formats) {
    p <- paste0(path_no_ext, ".pdf")
    ggplot2::ggsave(p, plot, width = width, height = height, device = grDevices::cairo_pdf)
    paths["pdf"] <- p
  }
  invisible(paths)
}
