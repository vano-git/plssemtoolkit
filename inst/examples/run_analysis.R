# ============================================================
# Example: running the full PLS-SEM pipeline on a new dataset
# ============================================================
#
# This is the ONLY file you should need to write/edit for a new project.
# Everything else (estimation, diagnostics, figures) lives in the package
# and is reused unchanged.
#
# Steps to adapt this to your own study:
#   1. Copy inst/extdata/example_model_spec.yml somewhere, rename
#      constructs/items to match your questionnaire's column headers.
#   2. Point `data` at your CSV.
#   3. Run.

# install.packages("devtools")
# devtools::install_github("YOUR_USERNAME/plssemtools")

library(plssemtools)

result <- run_pls_pipeline(
  data       = "path/to/your_survey_data.csv",
  spec       = "path/to/your_model_spec.yml",
  output_dir = "pls_output",
  nboot      = 5000,     # use 5000-10000 for a final manuscript run
  q2_folds   = 10,
  alpha      = 0.05,
  seed       = 1234,     # reproducibility: same seed -> same bootstrap CIs
  theme      = plssem_theme(base_family = "serif"),  # "serif" ~ Times-like
  formats    = c("png", "pdf")
)

# Everything is written to pls_output/tables/*.csv and
# pls_output/figures/*.{png,pdf}. You can also inspect results directly:

print(result)
result$model_summary$reliability
result$construct_descriptives

# Re-plot any single figure with different sizing/theme without re-running
# the whole model:
p <- plot_path_diagram(result$model_summary, result$spec, boot_summary = result$boot$summary)
p
