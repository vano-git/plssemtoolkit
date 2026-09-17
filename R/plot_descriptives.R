#' Plot construct means with SD error bars
#'
#' @param construct_desc Output of [compute_construct_descriptives()].
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_construct_descriptives <- function(construct_desc, theme = plssem_theme()) {
  ggplot2::ggplot(construct_desc, ggplot2::aes(x = stats::reorder(.data$Construct, .data$Mean), y = .data$Mean)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$Mean - .data$SD, ymax = .data$Mean + .data$SD),
                            width = 0.15, color = "gray40") +
    ggplot2::geom_point(size = 3, color = theme$palette$significant) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("M = %.2f\nSD = %.2f", .data$Mean, .data$SD)),
                        hjust = -0.25, size = 3, lineheight = 0.9) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(title = "Construct-Level Descriptive Statistics", x = NULL, y = "Mean (\u00b1 1 SD)") +
    theme$gg +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Generate and save the full set of publication-quality figures
#'
#' Calls every `plot_*` function in the package and writes each figure to
#' `output_dir` in the requested formats. This is what [run_pls_pipeline()]
#' calls internally; use it directly if you already have model objects from
#' your own code and just want the figures.
#'
#' @param model A `seminr_model`.
#' @param model_summary Output of `summary(model)`.
#' @param boot_summary Output of `summary(bootstrap_model(...))`. Optional --
#'   if NULL, the path diagram and bootstrap forest plot skip significance
#'   annotation / are omitted respectively.
#' @param spec A `pls_model_spec`.
#' @param construct_desc Optional output of
#'   [compute_construct_descriptives()]; skipped if NULL.
#' @param q2_table Optional output of [compute_q_squared()]; skipped if NULL.
#' @param output_dir Directory to write figures into.
#' @param theme A `plssem_theme`.
#' @param formats Character vector, subset of c("png", "pdf").
#' @return Invisibly, a named list of file paths written, one entry per
#'   figure (each entry itself a named vector of format -> path).
#' @export
generate_all_figures <- function(model, model_summary, boot_summary = NULL, spec,
                                  construct_desc = NULL, q2_table = NULL,
                                  output_dir = "figures", theme = plssem_theme(),
                                  formats = c("png", "pdf")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out <- list()

  safe_plot_save <- function(name, plot_fn, width, height) {
    result <- tryCatch({
      plot <- plot_fn()
      save_figure(plot, file.path(output_dir, name), width = width, height = height, formats = formats)
    }, error = function(e) {
      warning(sprintf("Skipped figure '%s': %s", name, conditionMessage(e)), call. = FALSE)
      NULL
    })
    out[[name]] <<- result
  }

  safe_plot_save("01_path_diagram", function() {
    plot_path_diagram(model_summary, spec, boot_summary = boot_summary, theme = theme)
  }, width = 8.5, height = 5.5)

  safe_plot_save("02_outer_loadings", function() {
    plot_loadings(model_summary, spec, theme = theme)
  }, width = 7, height = max(4, 0.35 * length(spec_all_items(spec))))

  safe_plot_save("03_htmt_heatmap", function() {
    plot_htmt_heatmap(model_summary, theme = theme)
  }, width = 6.5, height = 5.5)

  safe_plot_save("04_r_squared", function() {
    plot_r_squared(model_summary, spec, theme = theme)
  }, width = 6.5, height = 3 + 0.4 * length(unique(unlist(lapply(spec$paths, function(p) unlist(p$to))))))

  safe_plot_save("05_f_squared", function() {
    plot_f_squared(model_summary, theme = theme)
  }, width = 6.5, height = 5)

  if (!is.null(boot_summary)) {
    safe_plot_save("06_bootstrap_paths", function() {
      plot_bootstrap_paths(boot_summary, theme = theme)
    }, width = 7.5, height = 3 + 0.4 * nrow(as.data.frame(boot_summary$bootstrapped_paths)))
  }

  if (!is.null(q2_table)) {
    safe_plot_save("07_q_squared", function() {
      plot_q_squared(q2_table, theme = theme)
    }, width = 6.5, height = 3 + 0.35 * nrow(q2_table))
  }

  if (!is.null(construct_desc)) {
    safe_plot_save("08_construct_descriptives", function() {
      plot_construct_descriptives(construct_desc, theme = theme)
    }, width = 7, height = 3 + 0.4 * nrow(construct_desc))
  }

  invisible(out)
}
