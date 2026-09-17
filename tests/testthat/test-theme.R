test_that("plssem_theme returns a well-formed theme object", {
  th <- plssem_theme()
  expect_s3_class(th, "plssem_theme")
  expect_true(inherits(th$gg, "theme"))
  expect_true(is.list(th$palette))
  expect_true(all(c("significant", "nonsignificant") %in% names(th$palette)))
})

test_that("save_figure writes PNG and PDF files", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:3, y = c(2, 4, 3))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  paths <- save_figure(p, file.path(tmp_dir, "test_fig"), width = 4, height = 3,
                        formats = c("png", "pdf"))

  expect_true(file.exists(paths["png"]))
  expect_true(file.exists(paths["pdf"]))
})
