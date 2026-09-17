#' Construct-level descriptive statistics
#'
#' For each construct, computes the mean, SD, median, and mode of the
#' per-respondent average across that construct's items -- the same logic as
#' the original analysis script, generalized to any model spec.
#'
#' @param data A data.frame of survey responses.
#' @param spec A `pls_model_spec`.
#' @return A tibble with one row per construct: Construct, Mean, SD, Median,
#'   Mode, N.
#' @export
compute_construct_descriptives <- function(data, spec) {
  rows <- lapply(names(spec$constructs), function(construct_name) {
    items <- spec$constructs[[construct_name]]
    construct_data <- data[, items, drop = FALSE]
    row_means <- rowMeans(construct_data, na.rm = TRUE)

    tibble::tibble(
      Construct = construct_name,
      Mean = round(mean(row_means, na.rm = TRUE), 3),
      SD = round(stats::sd(row_means, na.rm = TRUE), 3),
      Median = round(stats::median(row_means, na.rm = TRUE), 3),
      Mode = round(.mode_of(round(row_means, 2)), 3),
      N = sum(!is.na(row_means))
    )
  })
  dplyr::bind_rows(rows)
}

.mode_of <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Item-level descriptive statistics
#'
#' Simple per-item mean/SD/min/max/skewness/kurtosis table, useful as a
#' supplementary appendix table in manuscripts.
#'
#' @param data A data.frame of survey responses.
#' @param spec A `pls_model_spec`.
#' @return A tibble with one row per item.
#' @export
compute_item_descriptives <- function(data, spec) {
  items <- spec_all_items(spec)
  construct_lookup <- unlist(lapply(names(spec$constructs), function(nm) {
    stats::setNames(rep(nm, length(spec$constructs[[nm]])), spec$constructs[[nm]])
  }))

  rows <- lapply(items, function(it) {
    x <- data[[it]]
    tibble::tibble(
      Construct = construct_lookup[[it]],
      Item = it,
      Mean = round(mean(x, na.rm = TRUE), 3),
      SD = round(stats::sd(x, na.rm = TRUE), 3),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Skewness = round(psych_skew(x), 3),
      Kurtosis = round(psych_kurtosi(x), 3),
      N = sum(!is.na(x))
    )
  })
  dplyr::bind_rows(rows)
}

# Lightweight skew/kurtosis so we don't force a hard dependency on `psych`
# for something this small (matches psych::skew(type=2)/kurtosi(type=2)).
psych_skew <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}
psych_kurtosi <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  num <- n * (n + 1) * sum(((x - m) / s)^4)
  denom <- (n - 1) * (n - 2) * (n - 3)
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  (num / denom) - term2
}
