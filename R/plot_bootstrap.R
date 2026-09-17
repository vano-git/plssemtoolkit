#' Plot bootstrapped path coefficients as a forest plot
#'
#' Shows each structural path's point estimate with its bootstrap confidence
#' interval, colored by significance. This is the standard "significance at
#' a glance" figure for PLS-SEM manuscripts, replacing a plain coefficient
#' table.
#'
#' @param boot_summary Output of `summary(bootstrap_model(...))`.
#' @param alpha Significance level used (for the plot subtitle only; the
#'   actual CI bounds come from whatever alpha the summary was computed at).
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_bootstrap_paths <- function(boot_summary, alpha = 0.05, theme = plssem_theme()) {
  boot_df <- as.data.frame(boot_summary$bootstrapped_paths)
  boot_df$Path <- rownames(boot_df)

  colnames(boot_df) <- gsub("\\s+", "_", colnames(boot_df))
  est_col <- grep("Original|Bootstrap_Mean|Est", colnames(boot_df), value = TRUE, ignore.case = TRUE)[1]
  lo_col <- grep("2\\.5%|CI_Lower|Lower", colnames(boot_df), value = TRUE, ignore.case = TRUE)[1]
  hi_col <- grep("97\\.5%|CI_Upper|Upper", colnames(boot_df), value = TRUE, ignore.case = TRUE)[1]
  t_col <- grep("T_Stat|T.Stat|t_value", colnames(boot_df), value = TRUE, ignore.case = TRUE)[1]

  if (any(is.na(c(est_col, lo_col, hi_col)))) {
    stop("Could not identify estimate/CI columns in bootstrapped_paths. ",
         "Found columns: ", paste(colnames(boot_df), collapse = ", "), call. = FALSE)
  }

  df <- data.frame(
    Path = boot_df$Path,
    Estimate = boot_df[[est_col]],
    Lower = boot_df[[lo_col]],
    Upper = boot_df[[hi_col]],
    Significant = if (!is.na(t_col)) abs(boot_df[[t_col]]) > 1.96 else NA
  )
  df$Path <- factor(df$Path, levels = rev(df$Path))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Estimate, y = .data$Path)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$Lower, xmax = .data$Upper), height = 0.15, color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(color = .data$Significant), size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$Estimate)), vjust = -1, size = 3) +
    ggplot2::scale_color_manual(
      values = c(`TRUE` = theme$palette$significant, `FALSE` = theme$palette$nonsignificant),
      na.value = theme$palette$accent,
      labels = c(`TRUE` = "Significant (|t| > 1.96)", `FALSE` = "Not significant"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Bootstrapped Path Coefficients",
      subtitle = "Point estimate with bootstrap confidence interval",
      x = "Path Coefficient", y = NULL
    ) +
    theme$gg

  p
}

#' Plot Q-squared predictive relevance by item
#'
#' @param q2_table Output of [compute_q_squared()].
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_q_squared <- function(q2_table, theme = plssem_theme()) {
  q2_table$Relevant <- q2_table$Q_Squared > 0

  ggplot2::ggplot(q2_table, ggplot2::aes(x = stats::reorder(.data$Item, .data$Q_Squared), y = .data$Q_Squared)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$Relevant), width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$Q_Squared)),
                        hjust = ifelse(q2_table$Q_Squared >= 0, -0.2, 1.2), size = 3) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = theme$palette$significant, `FALSE` = theme$palette$nonsignificant),
      guide = "none"
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = expression("Predictive Relevance (" * Q^2 * ")"),
      subtitle = expression(Q^2 > 0 ~ "indicates predictive relevance"),
      x = NULL, y = expression(Q^2)
    ) +
    theme$gg +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
