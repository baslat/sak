#' Check if something is on the system path
#'
#' Check if something is on the system path. It does this by using the
#' `where.exe` utility.
#'
#' @param what (character) a string, typically to file
#' @param verbose (logical; default = `TRUE`) message the path location?
#'
#' @return logical, \code{TRUE} if a match is found
#' @export
#'
#' @examples
#' \dontrun{
#' # Is Rscript on path?
#' on_path(what = "Rscript")
#' }
on_path <- function(what, verbose = TRUE) {
    function_needs("fs")
    # where.exe is in C:\Windows\System32\where.exe, so should be usable
    res <- run_well("where.exe",
        args = what,
        capture_error = TRUE
    )

    if (verbose) {
        res %>%
            paste(collapse = "\n") %>%
            message()
    }

    any(fs::is_file(res))
}
