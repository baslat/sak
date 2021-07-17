#' Normalise ABS geography names
#'
#' This function renames and reclassifies the columns of a data.frame to make
#' them easier to work with. It will always reclassify any column with
#' \code{code} in the name or with some sort of year identifier (e.g.
#' \code{lga_name_2018}) to \code{character}. Optionally, it can remove these
#' year identifiers from the column names and/or rename the columns to lower
#' case.
#'
#' @param .data (\code{data.frame}) A data.frame with columns you want renamed
#' @param remove_year (logical; default = TRUE) Do you want to remove year
#'   identifies in the column names?
#' @param make_lower (logical; default = TRUE) Do you want to set all column
#'   names to lower case?
#'
#' @return returns the original \code{tibble} with the geographic column names
#'   standardised.
#' @export
#'
#' @examples
#' \dontrun{
#' absmapsdata::lga2018 %>% normalise_geo_names()
#' }
normalise_geo_names <- function(.data,
                                remove_year = TRUE,
                                make_lower = TRUE) {

  clean_dat <- .data %>%
    # Make columns characters, and exclude columns with area in the name
    dplyr::mutate(dplyr::across(
      c(tidyselect::matches("(_[0-9]{2,4}|[0-9]{2,4}|code)"),
        -tidyselect::matches("area")), # Not the area column
      as.character))

  # Remove "_NN"-"_NNNN" or "NN"-"NNNN" from column names
  if (remove_year) {
    clean_dat <- clean_dat %>%
      dplyr::rename_with(
      .fn = stringr::str_remove_all,
      pattern = "(_[0-9]{2,4}|[0-9]{2,4})",
      .cols = tidyselect::matches("(_[0-9]{2,4}|[0-9]{2,4})")
    )
  }

  if (make_lower) {
    clean_dat <- clean_dat %>%
      dplyr::rename_with(.fn = tolower,
                         .cols = tidyselect::everything())

  }
  clean_dat
}
