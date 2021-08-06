test_that("visualise_dependencies returns a visNetwork", {
  deps <- visualise_dependencies(testthat::auto_test)
  expect_s3_class(deps, "visNetwork")
})
