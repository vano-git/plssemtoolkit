#' Plot the HTMT discriminant validity matrix as a heatmap
#'
#' @param model_summary Output of `summary(seminr_model)`.
#' @param threshold Conservative HTMT threshold to annotate. Default 0.85
#'   (0.90 is the more liberal commonly-cited alternative).
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_htmt_heatmap <- function(model_summary, threshold = 0.85, theme = plssem_theme()) {
  htmt <- as.data.frame(model_summary$validity$htmt)
  htmt$Construct1 <- rownames(htmt)
  long <- tidyr::pivot_longer(htmt, cols = -"Construct1", names_to = "Construct2", values_to = "HTMT")
  long <- long[!is.na(long$HTMT) & long$HTMT != 0, ]

  order <- rownames(htmt)
  long$Construct1 <- factor(long$Construct1, levels = order)
  long$Construct2 <- factor(long$Construct2, levels = order)
  long$Flag <- long$HTMT >= threshold

  ggplot2::ggplot(long, ggplot2::aes(x = .data$Construct2, y = .data$Construct1, fill = .data$HTMT)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$HTMT),
                                     fontface = ifelse(.data$Flag, "bold", "plain")),
                        size = 3.2, color = "black") +
    ggplot2::scale_fill_gradient(low = theme$palette$fill_low, high = theme$palette$fill_high,
                                  limits = c(0, 1), name = "HTMT") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title = "Heterotrait-Monotrait Ratio (HTMT)",
      subtitle = sprintf("Bold values \u2265 %.2f threshold", threshold),
      x = NULL, y = NULL
    ) +
    theme$gg +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 0),
      legend.position = "right"
    )
}

#' Plot R-squared for endogenous constructs
#'
#' @param model_summary Output of `summary(seminr_model)`.
#' @param spec A `pls_model_spec`.
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_r_squared <- function(model_summary, spec, theme = plssem_theme()) {
  endogenous <- unique(unlist(lapply(spec$paths, function(p) unlist(p$to))))

  r2_vals <- vapply(endogenous, function(nm) .lookup_r_squared(model_summary, nm), numeric(1))

  df <- data.frame(Construct = endogenous, R2 = r2_vals)
  df <- df[!is.na(df$R2), ]

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$Construct, .data$R2), y = .data$R2)) +
    ggplot2::geom_col(fill = theme$palette$significant, width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$R2)), hjust = -0.2, size = 3.3) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(limits = c(0, max(df$R2, 0.1) * 1.2), labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(title = "R\u00b2 of Endogenous Constructs", x = NULL, y = expression(R^2)) +
    theme$gg +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot f-squared effect sizes as a heatmap
#'
#' @param model_summary Output of `summary(seminr_model)`.
#' @param theme A `plssem_theme`.
#' @return A ggplot object.
#' @export
plot_f_squared <- function(model_summary, theme = plssem_theme()) {
  f2 <- as.data.frame(model_summary$fSquare)
  f2$Predictor <- rownames(f2)
  long <- tidyr::pivot_longer(f2, cols = -"Predictor", names_to = "Outcome", values_to = "f2")
  long <- long[!is.na(long$f2) & long$f2 != 0, ]

  long$Magnitude <- cut(long$f2, breaks = c(-Inf, 0.02, 0.15, 0.35, Inf),
                         labels = c("negligible", "small", "medium", "large"))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$Outcome, y = .data$Predictor, fill = .data$f2)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f\n(%s)", .data$f2, .data$Magnitude)),
                        size = 2.9, lineheight = 0.9) +
    ggplot2::scale_fill_gradient(low = theme$palette$fill_low, high = theme$palette$fill_high,
                                  name = expression(f^2)) +
    ggplot2::labs(title = expression("Effect Sizes (" * f^2 * ")"), x = NULL, y = NULL) +
    theme$gg +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
}
