#' Ask the user if they want to proceed
#'
#' A convenience wrapper to prompt the user if they want to proceed.
#'
#' @param msg (string; default = "Do you want to proceed?") a message to the
#'   user, often something like "do you want to proceed and run `function()`?"
#'
#' @return a logical where `TRUE` if the user wants to proceed
#' @export
#'
#' @examples
#' \dontrun{
#' use_renv <- ask_to_proceed("Do you want to set up renv?")
#'
#' if (use_renv) {
#' 	setup_renv()
#' }
#' }
ask_to_proceed <- function(msg = "Do you want to proceed?") {
	# never proceed if running from a script
	if (!interactive()) {
		return(FALSE)
	}
	assertthat::assert_that(
		assertthat::is.string(msg),
		msg = "`msg` must be a string."
	)
	res <- utils::menu(
		choices = c("yes", "no"),
		title = msg
	)

	res == 1L
}
