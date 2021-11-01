test_that("testing package is styled correctly", {
  # Whole bunch of preliminary stuff copied off github to deal with `check`
  # making a copy of package into a nested folder and specifying file paths:
  # https://github.com/jimhester/lintr/issues/421
  pkgname <- "sak"
  lint_path <- path.expand(normalizePath(getwd(), winslash = "/"))

  find_root <- function(path, pkgname) {
    if ("DESCRIPTION" %in% dir(path)) {
      return(path.expand(normalizePath(path, winslash = "/")))
    } else if ("00_pkg_src" %in% dir(path)) {
      path <- file.path(path, "00_pkg_src", pkgname)
      return(path.expand(normalizePath(path, winslash = "/")))
    } else {
      find_root(dirname(path), pkgname)
    }
  }

  lint_path <- find_root(lint_path, pkgname)



  # Exclude general files from linting
  excluded_files <- lint_path %>%
    file.path(c("data", "docs", "inst", "man", "vignettes", "tests")) %>%
    purrr::map(list.files,
               recursive = TRUE,
               full.names = TRUE) %>%
    unlist()


  # This might now be redundent, but if FALSE linting tests are skipped
  Sys.setenv("NOT_CRAN" = "true")

  # Single expectation to assume no liting
  lintr::expect_lint_free(lint_path,
                          # This bit is similar to the .lintr file, but the
                          # .lintr file is ignored on devops. So my workflow is
                          # running through the stages here
                          # https://github.com/jimhester/lintr#lintr-file-example
                          # until all good, then copying the linters from .lintr
                          # to here (including the NULLS)
                          # Using the dev version of lintr is giving a warning
                          # but still seems to be running:
                          # https://github.com/jimhester/lintr/pull/813
                          # Needs the dev version though otherwise when running
                          # check get errors about missing `source_file`
                          # argument
                          linters = lintr::with_defaults(
                            line_length_linter = NULL
                          ),
                          # Exclude specific files
                          exclusions = excluded_files)
})
