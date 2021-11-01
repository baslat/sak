#' Get geographical data from GitHub
#'
#' This function queries my personal GitHub and returns concorded geographic
#' data. It is used to concord geographies in \code{concord_geos}, and can be
#' used to get basic meshblock information like category or population estimate.
#'
#' @details
#' Available columns include ASGS, LGA, UCL, SUA, GCCSA, RA, CED and SED, and
#' more:
#' \itemize{
#' \item mb_code_2016
#' \item mb_category_name_2016
#' \item sa1_maincode_2016
#' \item sa1_7digitcode_2016
#' \item sa2_maincode_2016
#' \item sa2_5digitcode_2016
#' \item sa2_name_2016
#' \item sa3_code_2016
#' \item sa3_name_2016
#' \item sa4_code_2016
#' \item sa4_name_2016
#' \item gccsa_code_2016
#' \item gccsa_name_2016
#' \item state_code_2016
#' \item state_name_2016
#' \item ucl_code_2016
#' \item ucl_name_2016
#' \item sosr_code_2016
#' \item sosr_name_2016
#' \item sos_code_2016
#' \item sos_name_2016
#' \item sua_code_2016
#' \item sua_name_2016
#' \item lga_code_2018
#' \item lga_name_2018
#' \item poa_code_2016
#' \item ced_code_2018
#' \item ced_name_2018
#' \item sed_code_2020
#' \item sed_name_2020
#' \item ra_code_2016
#' \item ra_name_2016
#' \item area_albers_sqkm
#' \item dwellings
#' \item persons
#' }
#'
#' @param cols (unquoted character vector; default = \code{NULL}) the columns to
#'   select. They mostly follow the pattern of \code{geo_name_year} or
#'   \code{geo_code_year}, but see details for a full list. If \code{NULL} all
#'   available columns are returned.
#'
#' @return a \code{tibble} with as the columns you asked for the as many rows as
#'   needed to fulfil that request (i.e. up to 358,122 rows). Note that
#'   \code{dplyr::distinct()} is called before returning the results (to
#'   minimise redundant results).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_geos(cols = c(
#'   mb_category_name_2016,
#'   area_albers_sqkm,
#'   ced_name_2018,
#'   persons
#' ))
#' }
#'
get_geos <- function(cols = NULL) {

  # Basically need something that checks if there is a cached file in a env var path, and if so get it from there.
  dir <- Sys.getenv("R_SAK_PATH",
    unset = tempdir()
  )


  if (!geos_available()) {
    msg <- paste0(
      "Downloading and caching the ~120MB geos.csv file (to `",
      normalizePath(dir, winslash = "/"),
      "`). You can control where it it cached by setting the 'R_SAK_PATH' system variable."
    )
    message(msg)
    cache_geos()
  }

  geos_path <- file.path(dir, "geos.csv")


  col_expr <- rlang::enexpr(cols)

  if (is.null(col_expr)) {
    geos <- vroom::vroom(geos_path,
      show_col_types = FALSE
    ) %>%
      dplyr::distinct() %>%
      normalise_geo_names(remove_year = FALSE)
    return(geos)
  }

  vroom::vroom(geos_path,
    show_col_types = FALSE,
    col_select = {{ col_expr }}
  ) %>%
    dplyr::distinct() %>%
    normalise_geo_names(remove_year = FALSE)
}



#' Download and store the results of \code{get_geos}
#'
#'
#'
#' @param path where to store it, (should be inherited)
#'
#' @export
#'
cache_geos <- function(path = Sys.getenv("R_SAK_PATH",
                         unset = tempdir()
                       )) {
  assertthat::assert_that(dir.exists(path),
    msg = "`path` doesn't exist."
  )


  exists <- file.exists(file.path(path, "geos.csv"))
  if (exists) {
    resp <- utils::menu(c("Yes", "No"),
      title = "A file already exists, but for some reason I want to replace it. Can I overwrite it?"
    )

    if (resp == 2) {
      stop("Ok, bye.",
        call. = FALSE
      )
    }
  }

  download_file(
    url = "https://github.com/baslat/aus_geos_data/blob/master/geos.csv?raw=true",
    fileext = "csv",
    dir = path,
    name = "geos.csv"
  )

  # invisible(NULL)
}



#' Check if there geos file is available
#'
#' A coarse check to see if the geos file is available
#'
#' @param path (character) the directory where the geos file should be
#'
geos_available <- function(path = Sys.getenv("R_SAK_PATH",
                             unset = tempdir()
                           )) {
  file <- file.path(path, "geos.csv")

  file.exists(file) && (file.size(file) >= 125402818)
}




#' All geographies concordance
#'
#' Concord any geography to any other (well, most). It uses meshblocks to weight
#' (either by population or dwellings) and apportion values between geographies.
#' This does not return exactly the same values as official correspondence
#' tables provided by the ABS, in fact it systematically overestimates very low
#' percentages and underestimates very high percentages. The degree of the
#' discrepancy depends on the geographies being concorded, however I think it's
#' good enough for most use cases.
#'
#' Make sure your \code{.data}'s geo column names match with those available in
#' \code{\link{get_geos}}. There is no error checking for valid geo names
#' against what's available, yet.
#'
#' @param .data (\code{tibble}) with suitably named geo columns
#' @param ... columns to group by (e.g. date)
#' @param from_geo (unquoted character) the 'from' geography, present in both
#'   \code{.data} and returnable by \code{\link{get_geos}}
#' @param to_geo (unquoted character) the 'to' geography, present in both
#'   \code{.data} and returnable by \code{\link{get_geos}}
#' @param concord_wt (unquoted \code{persons} or \code{dwellings}) how to weight
#'   the meshblocks
#' @param value (unquoted character) the column in \code{.data} to concord
#'   across geographies
#' @param func (quoted character; default = \code{"sum"}) one of \code{"sum"} or
#'   \code{"mean"}, how should the \code{value} be concorded? If the quantity is
#'   additive, then \code{"sum"} makes sense. However if it is a rate or similar
#'   then \code{"mean"} (which is a weighted mean based on the
#'   \code{concord_wt}) might be more sensible.
#'
#' @return a tibble with columns for \code{...}, \code{to_geo} and \code{value}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Build your own concordance, for example between SA4 and LGA:
#'
#' sa4 <- absmapsdata::sa42016 %>%
#'   sf::st_set_geometry(NULL) %>% # Remove the geometry column
#'   select(starts_with("sa4")) %>% # Just keep the identifiers
#'   mutate(pct = 1) %>% # i.e. 100% of a SA4
#'   sak::normalise_geo_names(remove_year = FALSE) # deals with column classes
#'
#' sa4_lga_concord <- sa4 %>%
#'   concord_geos(sa4_name_2016, sa4_code_2016, # add the identifiers so they
#'     # are returned as columns
#'     from_geo = sa4_code_2016,
#'     to_geo = lga_name_2020,
#'     value = pct
#'   ) # i.e. the 100% from above
#' }
concord_geos <- function(.data,
                         ...,
                         from_geo,
                         to_geo,
                         concord_wt,
                         value,
                         func = "sum") {

  # # Check column names are valid
  test_wt <- as_label(rlang::enquo(concord_wt))
  test_from_geo <- as_label(rlang::enquo(from_geo))
  test_value <- as_label(rlang::enquo(value))

  assertthat::assert_that(any(test_wt %in% c("persons", "dwellings")),
    msg = "Check your `value for `concord_wt` argument, it must be one of `persons` or `dwellings` (unquoted)."
  )
  assertthat::assert_that(any(test_from_geo %in% names(.data)),
    msg = "Check your `from_geo` column is in your data."
  )
  assertthat::assert_that(any(test_value %in% names(.data)),
    msg = "Check your `value` column is in your data."
  )
  assertthat::assert_that(length(func) == 1,
    any(func == c("sum", "mean")),
    msg = "`func` must be one of `'sum'` or `'mean'`"
  )

  # Modify the all_goes data to just the geos that are needed, and calculate
  # weights
  geos <- get_geos(cols = c(
    {{ from_geo }},
    {{ to_geo }},
    {{ concord_wt }}
  )) %>%
    dplyr::add_count({{ from_geo }},
      wt = {{ concord_wt }},
      name = "from_geo_wt"
    ) %>%
    dplyr::count({{ from_geo }},
      {{ to_geo }},
      .data$from_geo_wt,
      wt = {{ concord_wt }},
      name = "shared_wts"
    ) %>%
    dplyr::transmute({{ from_geo }},
      {{ to_geo }},
      .data$from_geo_wt,
      from_geo_ratio = .data$shared_wts / .data$from_geo_wt
    )


  # Check if groups are required
  if (!missing(...)) {
    .data <- dplyr::group_by(.data, ..., .add = TRUE)
  }

  out <- .data %>%
    dplyr::left_join(geos,
      by = test_from_geo
    ) %>%
    tidyr::drop_na(.data$from_geo_ratio)

  # The simple, aggregation function
  if (func == "sum") {
    concorded <- out %>%
      # Calculate the in-share based on the additive value (ie the thing concorded)
      dplyr::mutate(in_share = .data$from_geo_ratio * {{ value }}) %>%
      dplyr::count({{ to_geo }},
        wt = .data$in_share
      ) %>%
      dplyr::rename({{ value }} := .data$n) %>%
      dplyr::ungroup()
  } # The weighted mean function
  else if (func == "mean") {
    concorded <- out %>%
      # Calculate the in-share based on the base weight
      dplyr::mutate(in_share = .data$from_geo_ratio * .data$from_geo_wt) %>%
      dplyr::group_by({{ to_geo }}, .add = TRUE) %>%
      dplyr::summarise({{ value }} := weighted.mean(
        x = {{ value }},
        w = .data$in_share,
        na.rm = TRUE
      ),
      groups = "drop"
      )
  }



  return(concorded)
}
