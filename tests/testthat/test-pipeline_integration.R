test_that("run_pls_pipeline runs end to end on synthetic data and writes all outputs", {
  skip_on_cran() # slow: full PLS estimation + bootstrap + prediction
  spec <- make_test_spec()
  df <- make_synthetic_data(n = 200)
  tmp_csv <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_csv, row.names = FALSE)

  out_dir <- tempfile()

  result <- run_pls_pipeline(
    data = tmp_csv,
    spec = spec,
    output_dir = out_dir,
    nboot = 50,       # small for test speed; real runs should use >= 1000
    q2_folds = 3,
    seed = 1,
    formats = c("png"),
    verbose = FALSE
  )

  expect_s3_class(result, "pls_pipeline_result")
  expect_true(dir.exists(file.path(out_dir, "tables")))
  expect_true(dir.exists(file.path(out_dir, "figures")))
  expect_true(file.exists(file.path(out_dir, "tables", "path_coefficients.csv")))
  expect_true(file.exists(file.path(out_dir, "tables", "reliability_and_validity.csv")))
  expect_true(file.exists(file.path(out_dir, "figures", "01_path_diagram.png")))
  expect_true(file.exists(file.path(out_dir, "figures", "02_outer_loadings.png")))
})
