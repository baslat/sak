test_that("testing package is linted correctly", {
  lintr::expect_lint_free(exclusions = list("inst", "tests", "data-raw"))
})
