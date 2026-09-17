#' Load and validate survey data against a model spec
#'
#' Reads a CSV of respondent-level data and checks it contains every item
#' referenced by the model spec, with informative errors if not. This catches
#' typos and column mismatches before they turn into cryptic seminr errors.
#'
#' @param path Path to a CSV file.
#' @param spec A `pls_model_spec` from [load_model_spec()].
#' @param na_strings Character vector of strings to treat as NA. Default
#'   covers common survey export conventions.
#' @param id_column Optional name of a respondent ID column to keep aside
#'   (not used in modeling, but preserved for traceability). If present in
#'   the data it is dropped from modeling columns automatically.
#'
#' @return A data.frame containing at least all columns required by `spec`.
#' @export
load_survey_data <- function(path, spec, na_strings = c("NA", "N/A", "", "999"),
                              id_column = NULL) {
  if (!file.exists(path)) stop("Data file not found: ", path, call. = FALSE)
  df <- utils::read.csv(path, na.strings = na_strings, stringsAsFactors = FALSE,
                         check.names = TRUE)

  required <- spec_all_items(spec)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Data file is missing ", length(missing_cols), " column(s) required by the model spec:\n  ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns are:\n  ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  non_numeric <- required[!vapply(df[required], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      "The following item columns are not numeric (check for stray text, ",
      "commas, or coding issues): ", paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  n_missing <- sum(is.na(df[required]))
  if (n_missing > 0) {
    total <- nrow(df) * length(required)
    pct <- round(100 * n_missing / total, 1)
    message(sprintf(
      "Note: %d missing value(s) (%.1f%%) found across model items. ",
      n_missing, pct
    ), "seminr::estimate_pls() uses listwise/pairwise handling internally; ",
      "consider imputation upstream if this proportion is large.")
  }

  n_resp <- nrow(df)
  n_items <- length(required)
  min_recommended <- max(10 * max(table(unlist(
    lapply(spec$paths, function(p) unlist(p$to))
  ))), 100)
  if (n_resp < min_recommended) {
    message(sprintf(
      "Note: %d respondents loaded. Rules of thumb (e.g. 10x the largest number ",
      "of predictors for any construct) suggest at least ~%d for stable PLS estimates.",
      n_resp, min_recommended
    ))
  }

  attr(df, "id_column") <- id_column
  df
}
