#' Sui generis: show the unique values of character and factor columns
#'
#' Ever get a new dataset and you just want to know all the unique values for
#' the character and/or factor columns? Perhaps so that you can filter or
#' summarise or whatnot on the actual data you need? Well this function will do
#' that for you.
#'
#' @param .data a \code{data.frame}
#'
#' @return a \code{list} where each element is a single column \code{tibble},
#'   referring to a character column o f\code{.data}, and contains the unique
#'   values
#' @export
#'
#' @examples
#' \dontrun{
#' sg(dplyr::storms)
#' }
sg <- function(.data) {
  assertthat::assert_that(is.data.frame(.data),
                          msg = "`.data` must be a `data.frame`")

  .data %>%
    dplyr::select(tidyselect::vars_select_helpers$where(is.character),
                  tidyselect::vars_select_helpers$where(is.factor)) %>%
    names() %>%
    purrr::map(~unique(.data[.]))
}
