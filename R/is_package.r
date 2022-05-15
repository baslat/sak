#' Is the current project a package?
#'
#' Very simple wrapper to see if the current project is a package.
#'
#' @return `logical`
#'
is_package <- function() {
    desc::desc_has_fields("Package")
}
