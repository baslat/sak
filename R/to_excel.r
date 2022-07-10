#' Send a `data.frame` to Excel
#'
#' This file will open a `data.frame` in a temporary Excel csv. It won't do
#' anything in an automated script.
#'
#' @param .data a `data.frame`
#'
#' @return `.data` invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' dplyr::storms %>% to_excel()
#' }
to_excel <- function(.data) {
    rlang::check_installed("readr") # nolint
    # assert it's a df, and not sf
    assertthat::assert_that(is.data.frame(.data),
        msg = ".data needs to be a data.frame"
    )

    assertthat::assert_that(!inherits(.data, "sf"),
        msg = ".data cannot be an sf. Use strip_geometry() on it and try again."
    )

    if (interactive()) {
        tmp <- tempfile(fileext = ".csv")
        readr::write_excel_csv(.data, tmp)
        shell(tmp) # opens the temp file # nolint
    }
    invisible(.data)
}
