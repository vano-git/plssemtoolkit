test_that("load_model_spec builds a valid spec from a list", {
  spec <- make_test_spec()
  expect_s3_class(spec, "pls_model_spec")
  expect_equal(length(spec$constructs), 4)
  expect_equal(spec$constructs[["Psychological Factors"]], c("PS_1", "PS_2", "PS_3"))
})

test_that("load_model_spec errors on missing constructs", {
  expect_error(
    load_model_spec(list(paths = list(list(from = "A", to = "B")))),
    "at least one construct"
  )
})

test_that("load_model_spec errors on missing paths", {
  expect_error(
    load_model_spec(list(constructs = list("A" = list(type = "reflective", items = c("A_1"))))),
    "at least one path"
  )
})

test_that("load_model_spec errors when a path references an undefined construct", {
  expect_error(
    load_model_spec(list(
      constructs = list("A" = list(type = "reflective", items = c("A_1", "A_2"))),
      paths = list(list(from = "A", to = "Nonexistent"))
    )),
    "undefined construct"
  )
})

test_that("spec_all_items returns every item across constructs", {
  spec <- make_test_spec()
  items <- spec_all_items(spec)
  expect_equal(length(items), 3 + 4 + 3 + 5)
  expect_true(all(c("PS_1", "GV_5") %in% items))
})

test_that("load_model_spec rejects an invalid construct type", {
  expect_error(
    load_model_spec(list(
      constructs = list("A" = list(type = "bogus", items = c("A_1", "A_2"))),
      paths = list(list(from = "A", to = "A"))
    )),
    "reflective"
  )
})
