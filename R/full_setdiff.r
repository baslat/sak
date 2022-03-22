#' Calculate the full set difference between two objects
#'
#' Regular `setdiff()` is order-specific, but sometimes you just want to know what elements are just not in both lists. This function does that.
#'
#' @inheritParams base::setdiff
#'
#' @export
#'
full_setdiff <- function(x, y) {
    c(setdiff(x, y), setdiff(y, x))
}
