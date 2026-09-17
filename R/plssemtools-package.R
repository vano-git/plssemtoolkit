#' plssemtools: Reproducible PLS-SEM Analysis and Publication-Quality Figures
#'
#' A dataset-agnostic wrapper around the \pkg{seminr} package that turns a
#' one-off analysis script into a reusable, reproducible pipeline. Point it at
#' any CSV of survey responses and any construct/item specification (via a
#' YAML config or R list) and it will run the full PLS-SEM workflow --
#' descriptive statistics, measurement + structural model estimation,
#' reliability/validity diagnostics, bootstrapped significance testing,
#' blindfolding-based predictive relevance (Q-squared) -- and export both
#' machine-readable tables (CSV) and clean, journal-style ggplot2 figures
#' (PNG + PDF) ready to drop into a manuscript.
#'
#' @section Typical workflow:
#' \enumerate{
#'   \item \code{\link{load_model_spec}} -- read a YAML/JSON/list model spec
#'   \item \code{\link{run_pls_pipeline}} -- run the entire analysis end to end
#'   \item Individual \code{plot_*} functions if you want custom figures
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL
