#' Estimate a PLS-SEM model from a spec and data
#'
#' Thin, well-documented wrapper around [seminr::estimate_pls()].
#'
#' @param data A data.frame of survey responses (see [load_survey_data()]).
#' @param spec A `pls_model_spec` (see [load_model_spec()]).
#' @param ... Additional arguments passed to [seminr::estimate_pls()].
#' @return A `seminr_model` object.
#' @export
estimate_model <- function(data, spec, ...) {
  seminr::estimate_pls(
    data = data,
    measurement_model = spec$seminr_measurement,
    structural_model = spec$seminr_structural,
    ...
  )
}

#' Run bootstrapped resampling for significance testing
#'
#' @param model A `seminr_model` from [estimate_model()].
#' @param nboot Number of bootstrap resamples. 1000+ recommended for
#'   publication; 5000-10000 for final manuscript submission per current
#'   PLS-SEM guidance (Hair et al.).
#' @param alpha Significance level for the summary's confidence intervals.
#' @param seed Optional random seed for reproducibility. Strongly recommended
#'   -- without it, bootstrap CIs and p-values will differ slightly each run.
#' @param ... Additional arguments passed to [seminr::bootstrap_model()].
#' @return A list with `boot_model` (raw seminr object) and `summary`
#'   (its summary at the given alpha).
#' @export
run_bootstrap <- function(model, nboot = 1000, alpha = 0.05, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  boot_model <- seminr::bootstrap_model(seminr_model = model, nboot = nboot, ...)
  list(boot_model = boot_model, summary = summary(boot_model, alpha = alpha))
}

#' Compute Q-squared (predictive relevance) via k-fold prediction
#'
#' Generalizes the SSO/SSE/Q2 computation from the original script to work
#' for any endogenous construct(s) in the model spec, not just one hardcoded
#' construct.
#'
#' @param model A `seminr_model` from [estimate_model()].
#' @param data The data.frame used to estimate the model (needed to compute
#'   SSO against the raw item variance).
#' @param spec A `pls_model_spec`.
#' @param noFolds Number of folds for cross-validated prediction.
#' @param technique Prediction technique passed to [seminr::predict_pls()];
#'   defaults to `seminr::predict_DA`.
#' @param seed Optional random seed for reproducibility.
#' @return A tibble with columns Construct, Item, SSO, SSE, Q_Squared -- one
#'   row per item of every endogenous (i.e. "to") construct in the model.
#' @export
compute_q_squared <- function(model, data, spec, noFolds = 10,
                               technique = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(technique)) technique <- seminr::predict_DA

  endogenous <- unique(unlist(lapply(spec$paths, function(p) unlist(p$to))))

  prediction <- seminr::predict_pls(model = model, technique = technique, noFolds = noFolds)
  summary_predict <- summary(prediction)

  rows <- lapply(endogenous, function(construct_name) {
    target_items <- spec$constructs[[construct_name]]
    rmse_values <- summary_predict$PLS_out_of_sample["RMSE", target_items]
    n <- nrow(data)
    sse_values <- (rmse_values^2) * n

    actual_subset <- data[, target_items, drop = FALSE]
    sso_values <- vapply(actual_subset, function(x) {
      sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)
    }, numeric(1))

    q2_values <- 1 - (sse_values / sso_values)

    tibble::tibble(
      Construct = construct_name,
      Item = target_items,
      SSO = round(sso_values, 2),
      SSE = round(as.numeric(sse_values), 2),
      Q_Squared = round(as.numeric(q2_values), 3)
    )
  })

  dplyr::bind_rows(rows)
}

#' Run the full PLS-SEM analysis pipeline end to end
#'
#' The one-call entry point: loads/validates data against a model spec,
#' estimates the model, computes descriptives, reliability/validity tables,
#' Q-squared, and bootstrapped significance, exports every table as CSV, and
#' generates the full set of publication-quality figures. This is the
#' generalized, reusable replacement for the original monolithic script --
#' point it at a new dataset and a new (or the same) model spec and it
#' reproduces the entire workflow without any code changes.
#'
#' @param data Either a path to a CSV file or an already-loaded data.frame.
#' @param spec Either a path to a YAML/JSON model spec, an R list, or an
#'   already-loaded `pls_model_spec`.
#' @param output_dir Directory to write CSV tables and figures into. Created
#'   if it does not exist. Subfolders `tables/` and `figures/` are created
#'   inside it.
#' @param nboot Number of bootstrap resamples (default 1000; use 5000-10000
#'   for a final manuscript run).
#' @param q2_folds Number of folds for the Q-squared blindfolding-style
#'   prediction (default 10).
#' @param alpha Significance level for bootstrap confidence intervals.
#' @param seed Random seed for reproducibility of bootstrap and prediction
#'   steps. Default 1234; set to `NULL` to disable.
#' @param theme A `plssem_theme` from [plssem_theme()] controlling figure
#'   appearance. Defaults to the built-in clean academic theme.
#' @param formats Character vector of figure output formats: any of "png",
#'   "pdf". Default both.
#' @param verbose Print progress messages. Default TRUE.
#'
#' @return Invisibly, a list of class `pls_pipeline_result` containing every
#'   intermediate object (`data`, `spec`, `model`, `boot`, all tables, and the
#'   paths of every file written) so you can inspect or extend results
#'   programmatically after the call.
#'
#' @examples
#' \dontrun{
#' result <- run_pls_pipeline(
#'   data = "survey_data.csv",
#'   spec = "model_spec.yml",
#'   output_dir = "output",
#'   nboot = 5000,
#'   seed = 1234
#' )
#' }
#' @export
run_pls_pipeline <- function(data, spec, output_dir = "pls_output",
                              nboot = 1000, q2_folds = 10, alpha = 0.05,
                              seed = 1234, theme = plssem_theme(),
                              formats = c("png", "pdf"), verbose = TRUE) {

  .log <- function(...) if (verbose) message(...)

  spec <- if (inherits(spec, "pls_model_spec")) spec else load_model_spec(spec)

  data <- if (is.character(data)) {
    load_survey_data(data, spec)
  } else if (is.data.frame(data)) {
    data
  } else {
    stop("`data` must be a file path or a data.frame.", call. = FALSE)
  }

  dir_tables <- file.path(output_dir, "tables")
  dir_figures <- file.path(output_dir, "figures")
  dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)

  written <- list()
  .write_csv <- function(df, name) {
    p <- file.path(dir_tables, paste0(name, ".csv"))
    utils::write.csv(df, p, row.names = TRUE)
    written[[name]] <<- p
    p
  }

  .log("[1/7] Descriptive statistics...")
  construct_desc <- compute_construct_descriptives(data, spec)
  item_desc <- compute_item_descriptives(data, spec)
  .write_csv(construct_desc, "construct_descriptives")
  .write_csv(item_desc, "item_descriptives")

  .log("[2/7] Estimating PLS-SEM model...")
  model <- estimate_model(data, spec)
  model_summary <- summary(model)

  .log("[3/7] Reliability and validity diagnostics...")
  .write_csv(as.data.frame(model_summary$paths), "path_coefficients")
  .write_csv(as.data.frame(model_summary$fSquare), "effect_size_f2")
  .write_csv(as.data.frame(model_summary$loadings), "outer_loadings")
  .write_csv(as.data.frame(model_summary$reliability), "reliability_and_validity")
  .write_csv(as.data.frame(model_summary$validity$fl_criteria), "fornell_larcker_criterion")
  .write_csv(as.data.frame(model_summary$validity$htmt), "htmt_ratio")
  .write_csv(as.data.frame(model_summary$validity$cross_loadings), "cross_loadings")

  vif_path <- file.path(dir_tables, "vif_items.txt")
  writeLines(utils::capture.output(print(model_summary$validity$vif_items)), vif_path)
  written[["vif_items"]] <- vif_path

  .log("[4/7] Q-squared (predictive relevance)...")
  q2_table <- tryCatch(
    compute_q_squared(model, data, spec, noFolds = q2_folds, seed = seed),
    error = function(e) {
      warning("Q-squared computation failed: ", conditionMessage(e),
              ". Skipping -- check that your model has a categorical/DA-compatible ",
              "endogenous construct, or pass a different `technique` to compute_q_squared().",
              call. = FALSE)
      NULL
    }
  )
  if (!is.null(q2_table)) .write_csv(q2_table, "q_squared")

  .log(sprintf("[5/7] Bootstrapping (nboot = %d)...", nboot))
  boot <- run_bootstrap(model, nboot = nboot, alpha = alpha, seed = seed)
  .write_csv(as.data.frame(boot$summary$bootstrapped_paths), "bootstrapped_path_coefficients")
  .write_csv(as.data.frame(boot$summary$bootstrapped_loadings), "bootstrapped_loadings")

  .log("[6/7] Generating publication-quality figures...")
  fig_paths <- generate_all_figures(
    model = model, model_summary = model_summary, boot_summary = boot$summary,
    spec = spec, construct_desc = construct_desc, q2_table = q2_table,
    output_dir = dir_figures, theme = theme, formats = formats
  )

  .log("[7/7] Done.")

  result <- list(
    data = data,
    spec = spec,
    model = model,
    model_summary = model_summary,
    boot = boot,
    construct_descriptives = construct_desc,
    item_descriptives = item_desc,
    q2_table = q2_table,
    tables_written = written,
    figures_written = fig_paths,
    output_dir = output_dir
  )
  class(result) <- "pls_pipeline_result"
  .log(sprintf("All outputs written to: %s", normalizePath(output_dir, mustWork = FALSE)))
  invisible(result)
}

#' @export
print.pls_pipeline_result <- function(x, ...) {
  cat("<pls_pipeline_result>\n")
  cat("  Output directory:", x$output_dir, "\n")
  cat("  Tables written:", length(x$tables_written), "\n")
  cat("  Figures written:", length(x$figures_written), "\n")
  cat("  Constructs:", paste(names(x$spec$constructs), collapse = ", "), "\n")
  invisible(x)
}
