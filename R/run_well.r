#' A wrapper to run system commands well
#'
#' This function is a wrapper for `\link{[processx]{run}}`, with opinionated error
#' handling. It will return the result of `stdin` if possible, and give errors
#' about timeouts and `stderr` otherwise.
#'
#' @inheritParams processx::run
#' @param split_args (logical; default  = `TRUE`) split the `args` string into a
#'   vector on spaces (because that's how `processx::run` expects it)?
#' @param capture_error (logical; default = `FALSE`) return `stderr` as a
#'   character vector?
#' @param ... other arguments passed to `\link{[processx]{run}}`.
#'
#' @return a character vector of `stdout`
#' @export
#'
#' @examples
#' \dontrun{
#' # Where is R (hopefully on your path)?
#' run_well("where.exe", "R")
#'
#' # Where is are (hopefully nowhere)?
#' run_well("where.exe", "are")
#' }
run_well <- function(
	command,
	args = character(),
	split_args = TRUE,
	capture_error = FALSE,
	...
) {

 # processx requires the args to be split into vectors, but that is a lot of
	# extra punctuation, so this can do it automatically.
	rlang::check_installed("processx")
	# https://stackoverflow.com/questions/9577930/regular-expression-to-select-all-whitespace-that-isnt-in-quotes
	spaces_not_in_quotes <- "\\s+(?=([^\"]*\"[^\"]*\")*[^\"]*$)"

 if (split_args && !rlang::is_empty(args)) {
		args <- args %>%
			stringr::str_split(pattern = spaces_not_in_quotes) %>% # one or more spaces
			unlist()
	}


 res <- processx::run(
		command = command,
		args = args,
		...,
		error_on_status = FALSE
	)

 # If status is 0, then exit early and return stdout
 if (res[["status"]] == 0L) {
		# need the treatment to remove trailing gumf
		return(
			res[["stdout"]] %>%
				stringr::str_split(pattern = "\r\n") %>%
				unlist() %>%
				purrr::map_chr(stringr::str_squish) %>%
				purrr::compact() %>%
				purrr::discard(. == "")
		)
	}

 # Capture error if asked to
	if (capture_error) {
		return(
			res[["stderr"]] %>%
				stringr::str_split(pattern = "\r\n") %>%
				unlist() %>%
				purrr::map_chr(stringr::str_squish) %>%
				purrr::compact() %>%
				purrr::discard(. == "")
		)
	}


 # Use asserts for good errors
	# Did it timeout?
 assertthat::assert_that(
		!res[["timeout"]],
		msg = "Your process timed out."
	)

 # What called the process?
	# Use this convolution to get the name of the calling function
	calling_env <- deparse(sys.calls()[[max(1, sys.nframe() - 1)]]) # nolint

 # Did it fail otherwise?
	assertthat::assert_that(
		res[["status"]] == 0L,
		msg = glue::glue("Your process (`{calling_env}`) errored. Here is the error:
                                           {res[['stderr']]}")
	)
}
