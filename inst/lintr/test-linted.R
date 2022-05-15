test_that("testing package is styled correctly", {
  # Whole bunch of preliminary stuff copied off github to deal with `check`
  # making a copy of package into a nested folder and specifying file paths:
  # https://github.com/jimhester/lintr/issues/421
  pkgname <- "place_package_name_here_please"
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
    file.path(c("data", "docs", "inst", "man", "vignettes")) %>%
    purrr::map(list.files,
               recursive = TRUE,
               full.names = TRUE) %>%
    unlist()


  # This might now be redundant, but if FALSE linting tests are skipped
  Sys.setenv("NOT_CRAN" = "true")

  # Single expectation to assume no linting
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
                            # Default linters
                            trailing_whitespace_linter = NULL,
                            line_length_linter = NULL,
                            object_length_linter = lintr::object_length_linter(length = 50L),
                            object_name_linter = lintr::object_name_linter(styles = c("snake_case", "symbols")),
                            # Bonus linters
                            absolute_path_linter = lintr::absolute_path_linter(lax = TRUE),
                            backport_linter = lintr::backport_linter(),
                            duplicate_argument_linter = lintr::duplicate_argument_linter(),
                            missing_argument_linter = lintr::missing_argument_linter(),
                            namespace_linter = lintr::namespace_linter(),
                            nonportable_path_linter = lintr::nonportable_path_linter(),
                            sprintf_linter = lintr::sprintf_linter(),
                            todo_comment_linter = lintr::todo_comment_linter(),
                            undesirable_operator_linter = lintr::undesirable_operator_linter(),
                            unneeded_concatenation_linter = lintr::unneeded_concatenation_linter()),
                          # Exclude specific files
                          exclusions = excluded_files)
})
