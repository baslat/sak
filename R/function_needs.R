#' Conditionally use a function from suggested package
#'
#' Use case: You make a function that uses something from a package listed as
#' \code{suggested}. This means it is not guaranteed that the package will be
#' installed. Add \code{function_needs('package')} at the top of the function
#' you are writing. If \code{package} is installed there is no change of
#' behaviour. If not, an informative error message is provided. This function
#' abstracts away the complexity so it can be widely used.
#'
#' Threshold for use: If a package is used by only one function , it should
#' almost certainly be included under \code{suggests} in \code{DESCRIPTION}. If
#' it is used extensively it should be included under \code{imports} -- think
#' \code{dplyr}, \code{glue}, \code{purrr}, and \code{tibble}. Once it is being
#' used by several functions, used by different people then it's probably ready
#' for \code{imports}.
#'
#' @param pkg (quoted character) name of the package that your function requires
#'
#' @return TRUE if pkg is installed, else error message
#' @export
#' @examples \dontrun{
#'
#' function_that_uses_absmapsdata <- function() {
#'     function_needs("absmapsdata")
#'     print(absmapsdata::lga2018)
#'     }
#'
#' function_that_uses_absmapsdata()
#'
#' }
#'
function_needs <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue::glue("package {pkg} needed to use this function. Please install
                    with `install.packages('pkg')`, `remotes::install_git()`
                    or `remotes::install_github('username/repo')"))
  }
  return(TRUE)
}
