#' Plot outer loadings by construct
#'
#' A faceted, journal-style dot plot of item loadings by construct, with a
#' reference line at the conventional 0.70 acceptability threshold. Far
#' clearer for a reviewer to scan than a raw loadings table.
#'
#' @param model_summary Output of `summary(seminr_model)`.
#' @param spec A `pls_model_spec`.
#' @param threshold Loading threshold reference line. Default 0.70.
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_loadings <- function(model_summary, spec, threshold = 0.70, theme = plssem_theme()) {
  loadings <- as.data.frame(model_summary$loadings)
  loadings$Item <- rownames(loadings)

  construct_lookup <- unlist(lapply(names(spec$constructs), function(nm) {
    stats::setNames(rep(nm, length(spec$constructs[[nm]])), spec$constructs[[nm]])
  }))

  long <- tidyr::pivot_longer(
    loadings, cols = -"Item", names_to = "Construct", values_to = "Loading"
  )
  long <- long[!is.na(long$Loading) & long$Loading != 0, ]
  long$OwnConstruct <- construct_lookup[long$Item] == long$Construct
  long <- long[long$OwnConstruct, ]
  long$Item <- factor(long$Item, levels = spec_all_items(spec))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$Loading, y = .data$Item)) +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed",
                         color = "gray50", linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$Loading, y = .data$Item, yend = .data$Item),
                           color = "gray70", linewidth = 0.4) +
    ggplot2::geom_point(size = 2.6, color = theme$palette$significant) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$Loading)),
                        hjust = -0.35, size = 3) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Construct), scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(limits = c(0, 1.05), expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(
      title = "Outer Loadings by Construct",
      subtitle = sprintf("Dashed line = conventional acceptability threshold (%.2f)", threshold),
      x = "Standardized Loading", y = NULL
    ) +
    theme$gg +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
