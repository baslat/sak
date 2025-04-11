#' Set up \{targets\}
#'
#' Set up boilerplate for using \{targets\}.
#'
#' @return nothing, used for side effects
#'
#' @export
#'
setup_targets <- function() {
	# inspired by `tflow::use_tflow()`
	usethis::use_directory("R")
	usethis::use_template(
		template = "_packages.R",
		save_as = file.path("R", "_packages.R"),
		package = "sak"
	)
	usethis::use_template("_targets.R", package = "sak")
	usethis::use_template(
		template = "dot.env",
		save_as = ".env",
		package = "sak"
	)

	usethis::use_template("control.R", package = "sak")
}
