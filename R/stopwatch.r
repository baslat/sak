#' Time an expression
#'
#' This is a wrapper for `system.time`, but returns only the total elapsed time.
#'
#' @inheritParams base::system.time
#'
#' @return (`lubridate::period`) total elapsed time
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # This will return time_taken and a_result
#' time_taken <- stopwatch({
#'     a_result <- Sys.sleep(60)
#' })
#' }
stopwatch <- function(expr) {
    # Time the expression
    time <- system.time(expr)
    time <- time[3]
    lubridate::seconds_to_period(time)
}
