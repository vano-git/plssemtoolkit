test_that("compute_construct_descriptives returns one row per construct with expected columns", {
  spec <- make_test_spec()
  df <- make_synthetic_data()

  desc <- compute_construct_descriptives(df, spec)
  expect_equal(nrow(desc), 4)
  expect_setequal(names(desc), c("Construct", "Mean", "SD", "Median", "Mode", "N"))
  expect_true(all(desc$N == nrow(df)))
  expect_true(all(desc$Mean > 0 & desc$Mean < 8))
})

test_that("compute_item_descriptives returns one row per item", {
  spec <- make_test_spec()
  df <- make_synthetic_data()

  desc <- compute_item_descriptives(df, spec)
  expect_equal(nrow(desc), length(spec_all_items(spec)))
  expect_true(all(c("Mean", "SD", "Skewness", "Kurtosis") %in% names(desc)))
})

test_that(".mode_of returns the most frequent value", {
  expect_equal(.mode_of(c(1, 2, 2, 3)), 2)
  expect_true(is.na(.mode_of(c(NA, NA))))
})
