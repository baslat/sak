#' Setup the \code{lintr} config file
#'
#' A simple function to create the config file needed for \code{lintr}. Most
#' linters are enable by default, except for \code{line_length_linter} and
#' \code{trailing_whitespace_linter}.
#'
#' @inheritDotParams lintr::use_lintr
#'
#' @return Nothing, called for side effects.
#' @export
setup_lintr_config <- function(...) {
  rlang::check_installed("lintr")
  lintr::use_lintr(...)
  }

#' Create `{lintr}` test that file
#'
#' This function will take the file test-linted.R from the system files of the
#' package and copy it into the test/testthat directory of your package. This
#' will allow lintr tests to be executed when you run check on your package.
#'
#' @export
#'
setup_lintr_testthat <- function() {

  usethis::use_template(
    template = "test-linted.R",
    save_as = file.path("tests", "testthat", "test-linted.R"),
    package = "sak"
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

  assertthat::assert_that(
    default_branch %in% c("master", "main"),
    msg = "`default_branch` should be 'main' or 'master'"
  )

  usethis::use_template(
    template = "mega-linter.yml",
    save_as = ".mega-linter.yml",
    data = list(branch = default_branch)
  )
}
