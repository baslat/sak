#' Check if something is on the system path
#'
#' Check if something is on the system path. It does this by using the
#' `where.exe` utility.
#'
#' @param what (character) a string, typically to file ,
#'   where backslashes are replaced with double underscores!
#' @param verbose (logical; default = `TRUE`) message the path location?
#'
#' @return logical, \code{TRUE} if a match is found
#' @export
#'
#' @examples
#' \dontrun{
#' # Is Rscript
#' on_path(what = "Rscript")
#' }
on_path <- function(what, verbose = TRUE) {
    lifecycle::signal_stage("experimental", "on_path()")

    # only runs on windows
    if (.Platform[["OS.type"]] != "windows") {
        message("This only runs on Windows")
        invisible(FALSE)
    }

    # where.exe is in C:\Windows\System32\where.exe, so should be usable
    res <- suppressWarnings(
        system2("where.exe",
            args = what,
            stdout = TRUE,
            stderr = NULL
        )
    ) %>%
        strsplit(";")

    if (verbose) {
        res %>%
            paste(collapse = "\n") %>%
            message()
    }

    !rlang::is_empty(res)
}
