#' Plot the structural model path diagram
#'
#' Draws a clean, black-and-white academic path diagram: exogenous
#' constructs as boxes on the left, endogenous construct(s) on the right,
#' arrows labeled with standardized path coefficients (and significance
#' stars if a bootstrap summary is supplied), and R-squared shown inside
#' each endogenous construct's box. This replaces seminr's default
#' `plot(pls_model)` base-R graphic, which is not publication-formatted.
#'
#' @param model_summary Output of `summary(seminr_model)`.
#' @param spec A `pls_model_spec`.
#' @param boot_summary Optional output of
#'   `summary(bootstrap_model(...))`; if supplied, significance stars
#'   (*, **, ***) are added to path labels based on the bootstrap p-values.
#' @param theme A `plssem_theme`.
#' @return A ggplot object (built on geom primitives so it exports cleanly
#'   to PDF/PNG at any size).
#' @export
plot_path_diagram <- function(model_summary, spec, boot_summary = NULL,
                               theme = plssem_theme()) {

  exogenous <- setdiff(names(spec$constructs),
                        unique(unlist(lapply(spec$paths, function(p) unlist(p$to)))))
  endogenous <- unique(unlist(lapply(spec$paths, function(p) unlist(p$to))))

  path_df <- as.data.frame(model_summary$paths)
  path_df$from_to <- rownames(path_df)

  edges <- do.call(rbind, lapply(spec$paths, function(p) {
    expand.grid(from = unlist(p$from), to = unlist(p$to), stringsAsFactors = FALSE)
  }))

  get_coef <- function(from, to) {
    hit <- rownames(path_df)[stringr::str_detect(rownames(path_df), stringr::fixed(from)) &
                              stringr::str_detect(rownames(path_df), stringr::fixed(to))]
    if (length(hit) == 0) return(NA_real_)
    coef_col <- grep("Path Coeff|Original|Estimate", colnames(path_df), value = TRUE, ignore.case = TRUE)
    if (length(coef_col) == 0) coef_col <- colnames(path_df)[1]
    path_df[hit[1], coef_col[1]]
  }

  edges$coef <- mapply(get_coef, edges$from, edges$to)

  if (!is.null(boot_summary)) {
    boot_df <- as.data.frame(boot_summary$bootstrapped_paths)
    get_p <- function(from, to) {
      hit <- rownames(boot_df)[stringr::str_detect(rownames(boot_df), stringr::fixed(from)) &
                                stringr::str_detect(rownames(boot_df), stringr::fixed(to))]
      if (length(hit) == 0) return(NA_real_)
      colname <- grep("T Stat|p.value|p-value|P value", colnames(boot_df), value = TRUE, ignore.case = TRUE)
      if (length(colname) == 0) return(NA_real_)
      boot_df[hit[1], colname[1]]
    }
    edges$stat <- mapply(get_p, edges$from, edges$to)
    edges$sig_label <- ifelse(is.na(edges$stat), "",
                        ifelse(abs(edges$stat) > 2.58, "***",
                        ifelse(abs(edges$stat) > 1.96, "**",
                        ifelse(abs(edges$stat) > 1.65, "*", ""))))
  } else {
    edges$sig_label <- ""
  }

  edges$label <- sprintf("%.3f%s", edges$coef, edges$sig_label)

  n_exo <- length(exogenous)
  n_endo <- length(endogenous)
  exo_y <- seq(n_exo, 1, length.out = n_exo)
  endo_y <- seq(n_endo, 1, length.out = n_endo) * (n_exo / max(n_endo, 1))
  exo_y <- (exo_y - mean(exo_y)) + mean(range(1, n_exo))
  endo_y <- (endo_y - mean(endo_y)) + mean(range(1, n_exo))

  node_df <- rbind(
    data.frame(name = exogenous, x = 0, y = exo_y, role = "exogenous"),
    data.frame(name = endogenous, x = 4, y = mean(exo_y), role = "endogenous")
  )

  r2_text <- vapply(endogenous, function(nm) {
    val <- .lookup_r_squared(model_summary, nm)
    if (is.na(val)) "" else sprintf("R\u00b2 = %.3f", val)
  }, character(1))
  names(r2_text) <- endogenous

  node_df$label <- ifelse(node_df$role == "endogenous",
                           paste0(node_df$name, "\n", r2_text[node_df$name]),
                           node_df$name)

  edges_pos <- merge(edges, node_df[node_df$role == "exogenous", c("name", "x", "y")],
                      by.x = "from", by.y = "name")
  names(edges_pos)[names(edges_pos) == "x"] <- "x_start"
  names(edges_pos)[names(edges_pos) == "y"] <- "y_start"
  edges_pos <- merge(edges_pos, node_df[node_df$role == "endogenous", c("name", "x", "y")],
                      by.x = "to", by.y = "name")
  names(edges_pos)[names(edges_pos) == "x"] <- "x_end"
  names(edges_pos)[names(edges_pos) == "y"] <- "y_end"

  box_w <- 1.7
  box_h <- 0.6

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges_pos,
      ggplot2::aes(x = .data$x_start + box_w / 2, y = .data$y_start,
                   xend = .data$x_end - box_w / 2, yend = .data$y_end,
                   linewidth = abs(.data$coef)),
      color = theme$palette$significant,
      arrow = grid::arrow(length = grid::unit(0.18, "inches"), type = "closed")
    ) +
    ggplot2::geom_label(
      data = edges_pos,
      ggplot2::aes(x = (.data$x_start + .data$x_end) / 2,
                   y = (.data$y_start + .data$y_end) / 2, label = .data$label),
      size = 3.1, label.size = 0, fill = "white", fontface = "bold"
    ) +
    ggplot2::geom_tile(
      data = node_df, ggplot2::aes(x = .data$x, y = .data$y),
      width = box_w, height = box_h, fill = "white",
      color = "black", linewidth = 0.6
    ) +
    ggplot2::geom_text(
      data = node_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3.3, lineheight = 0.9, fontface = "bold"
    ) +
    ggplot2::scale_linewidth(range = c(0.4, 2.2), guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_family = theme$base_family, base_size = theme$base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.caption = ggplot2::element_text(color = "gray40", size = ggplot2::rel(0.75), hjust = 0)
    ) +
    ggplot2::labs(
      title = "Structural Model: Path Coefficients",
      caption = if (!is.null(boot_summary)) {
        "* p < .10   ** p < .05   *** p < .01 (based on bootstrap t-statistics)"
      } else {
        NULL
      }
    )

  p
}

#' Robustly extract R-squared for a construct from a seminr paths summary
#'
#' `seminr`'s `summary(model)$paths` matrix layout has varied slightly across
#' versions (row name "R^2" vs "R^2 " vs list-column access). This helper
#' tries several access patterns before giving up, so figure generation
#' degrades gracefully (blank R\u00b2 label) instead of erroring out the whole
#' pipeline.
#' @keywords internal
#' @noRd
.lookup_r_squared <- function(model_summary, construct_name) {
  paths_tab <- model_summary$paths
  row_candidates <- c("R^2", "R^2 ", "R2", "R_squared")
  for (rn in row_candidates) {
    if (rn %in% rownames(paths_tab)) {
      val <- tryCatch(as.numeric(paths_tab[rn, construct_name]), error = function(e) NA_real_)
      if (!is.na(val)) return(val)
    }
  }
  # Fallback: some seminr versions expose it via model_summary$paths attr or
  # a separate $total_effects / $r_squared element.
  if (!is.null(model_summary$r_squared)) {
    val <- tryCatch(as.numeric(model_summary$r_squared[construct_name]), error = function(e) NA_real_)
    if (!is.na(val)) return(val)
  }
  NA_real_
}
