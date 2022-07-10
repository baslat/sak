#' Is the current project a package?
#'
#' Very simple wrapper to see if the current project is a package.
#'
#' @return `logical`
#'
is_package <- function() {
    safe_field <- purrr::safely(desc::desc_has_fields, otherwise = FALSE)
    res <- safe_field("Package")
    res$result
}
