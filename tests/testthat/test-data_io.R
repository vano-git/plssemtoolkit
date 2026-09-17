test_that("load_survey_data errors informatively on missing columns", {
  spec <- make_test_spec()
  df <- make_synthetic_data()
  df$PS_1 <- NULL
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)

  expect_error(load_survey_data(tmp, spec), "missing 1 column")
})

test_that("load_survey_data errors on non-numeric item columns", {
  spec <- make_test_spec()
  df <- make_synthetic_data()
  df$PS_1 <- as.character(df$PS_1)
  df$PS_1[1] <- "not a number"
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)

  expect_error(load_survey_data(tmp, spec), "not numeric")
})

test_that("load_survey_data loads a well-formed file successfully", {
  spec <- make_test_spec()
  df <- make_synthetic_data()
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)

  loaded <- load_survey_data(tmp, spec)
  expect_true(all(spec_all_items(spec) %in% names(loaded)))
  expect_equal(nrow(loaded), nrow(df))
})
