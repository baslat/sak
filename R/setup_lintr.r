#' Setup the \code{lintr} config file
#'
#' A simple function to create the config file needed for \code{lintr}. Most
#' linters are enable by default, except for \code{line_length_linter} and
#' \code{trailing_whitespace_linter}.
#'
#' @return Nothing, called for side effects.
#' @export
setup_lintr_config <- function() {
  lintr_config <- file.path("lintr", "lintr_config.txt") |>
    system.file(package = "sak") |>
    readLines()

  usethis::write_over(
    path = ".lintr",
    lines = lintr_config
  )

  # If it's a package, edit the build ignore and add .lintr as a dep
  if (is_package()) {
    # modify .Rbuildignore to suppress warnings
    usethis::write_union(
      path = ".Rbuildignore",
      lines = "^.lintr"
    )

    usethis::use_package("lintr", type = "Suggests")
  }




  invisible(NULL)
}

#' Create Lintr test that file
#'
#' This function will take the file test-linted.R from the system files of the package
#' and copy it into the test/testthat directory of your package. This will allow lintr
#' tests to be executed when you run check on your package.
#'
#' @export
#'
setup_lintr_testthat <- function() {

  # Look at the system file and read the lines of the test-linted.R file
  lintr_file <- file.path("lintr", "test-linted.R") |>
    system.file(package = "sak")

  # Get the name of the Package we are working in from Description File
  package_name <- desc_get(keys = "Package") |>
    as.character()

  # Replace the package name in the lintr test-that file.
  lintr_file_lines <- readLines(lintr_file) |>
    stringr::str_replace(
      pattern = "place_package_name_here_please",
      replacement = package_name
    )

  usethis::write_over(
    path = file.path("tests", "testthat", "test-linted.R"),
    lines = lintr_file_lines
  )
}


#' Setup a yaml file for megalinter
#'
#' Megalinter is a pipeline program that can lint multiple languages. This
#' function creates a yaml file in the project working directory that configures
#' megalinter to run no AZDO pipelines.
#'
#' @param default_branch (str, default = \code{NULL}) the default branch of the
#'   repo, one of 'main' or 'master'. If \code{NULL} it will try to figure it
#'   out.
#'
#' @return nothing, called for side effects
#' @export
#'
setup_yaml_megalinter <- function(default_branch = NULL) {
  default_branch <- default_branch %||% usethis::git_branch_default()

  assertthat::assert_that(default_branch %in% c(
    "master",
    "main"
  ),
  msg = "`default_branch` should be 'main' or 'master'"
  )

  # Get the guts of the mega-linter file from inside `sak`
  # and replace the default_repo
  # and write the file
  ml_yaml <- file.path("yaml_files", "mega-linter.yml") |>
    system.file(package = "sak") %>%
    readLines() %>%
    stringr::str_replace(
      pattern = "MAIN_REPO",
      replacement = default_branch
    )

  usethis::write_over(
    path = ".mega-linter.yml",
    lines = ml_yaml
  )
}
