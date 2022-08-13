#' Use \code{pmap} to create a new column in a data.frame
#'
#' Sometimes you want to use \code{mutate} to create a new column in a
#' data.frame, but that function doesn't work with vectors. One workaround is to
#' use a \code{rowwise() \%>\% mutate()}, however that is quite slow. This
#' function lets you use the speed of \code{pmap} in a \code{mutate}-like syntax
#' (and returns the original data.frame with a new column, instead of just a
#' list). This works best if the column names of \code{.data} match the
#' arguments expected by \code{.f}.
#'
#' @inheritParams purrr::pmap
#' @inheritParams dplyr::mutate
#'
#' @param col_name (unquoted character) the name of the new column you want to
#'   add
#'
#' @return \code{.data} with a new column named after \code{col_name}.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Create a column of charts:
#'
#' # Create a custom plotting function
#' plot_starwars <- function(data) {
#'   ggplot2::qplot(x = mass, y = height, data = data)
#' }
#'
#' # Make a tibble with the starwars data in twice
#' tibble::tibble(data = list(
#'   dplyr::starwars,
#'   dplyr::starwars
#' )) %>%
#'   mutate_pmap(
#'     col_name = plot,
#'     .f = plot_starwars
#'   )
#' }
mutate_pmap <- function(.data,
                        col_name,
                        .f,
                        ...) {

  # Check column names of data match the arguments expected by the function
  nm <- names(.data)
  args <- rlang::fn_fmls_names(fn = .f)

  correct_args <- any(nm %in% args)


  assertthat::assert_that(correct_args,
    msg <- "Note that the column names in `.data` don't full match the expected arguments in `.f`. You might want to rename some columns." # nolint
  )


  out <- .data %>%
    dplyr::select(tidyselect::any_of(args)) %>%
    purrr::pmap(.f)

  .data %>%
    dplyr::mutate({{ col_name }} := out)
}
