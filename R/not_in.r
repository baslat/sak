#' Not `in`
#'
#' Oh how I was there was a \code{!%in%}, so this does the trick.
#'
#' @inheritParams base::`%in%`
#'
#' @export
#'
`%not_in%` <- function(x, table) {
    !(`%in%`(x, table))
}
