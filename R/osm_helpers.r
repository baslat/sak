#' Query OpenStreeMap (OSM) for features
#'
#' Get the SF details for each requested feature. The list of OSM features is
#' \href{https://wiki.openstreetmap.org/wiki/Map_features}{here}. Use in
#' conjunction with \code{osm_bind_features}.
#'
#' @param bb (bounding box) the area to be queried
#' @param feature_key (character) the feature key to query. Feature keys can be
#'   found here \code{osmdata::available_features()}
#' @param feature_values (character) a vector of feature values to query, passed
#'   to \code{osmdata::add_osm_feature()}. Use \code{NULL} to return all values
#'   of your feature.
#'
#' @return a list of \code{sf} objects, technically an \code{osmdata} object
#' @export
#'
#' @examples
#' \dontrun{
#' bs <- sf::st_read(covidpmc::onedrive("data/fires_storage/burnscar/lga_bs/lga_burnscar.shp")) %>%
#'     sf::st_transform(crs = 7844) %>%
#'     cah::normalise_geo_names() %>%
#'     dplyr::filter(stringr::str_detect(lga_name, "Kangaroo Island"))
#'
#' # KI BS bounding box
#' bb <- expand_bbox(bs, exp_factor = 0)
#'
#' # Query for main roads
#' features_raw <- osm_query_features(bb,
#'     feature_key = "highway",
#'     feature_values = c("motorway", "primary")
#' )
#'
#' # Collapse all the returned sfs into one
#' features <- osm_bind_features(features_raw,
#'     types = c("lines", "multilines")
#' )
#'
#' # Take a look if you're curious
#' mapview::mapview(features,
#'     zcol = "highway"
#' )
#'
#' # Intersect with the burnscar
#' burnscar_features <- features %>%
#'     cah::st_intersection_quicker(bs)
#'
#' # Summarise the potential effect
#' osm_summarise_features(burnscar_features,
#'     highway,
#'     .f = "length",
#'     units = "km"
#' )
#' }
osm_find <- function(bb,
                     feature_key,
                     feature_values) {

    # Check for installed package
    function_needs("osmdata") # nolint

    # Check CRS of bounding box
    bb_crs <- sf::st_crs(bb)

    assertthat::assert_that(bb_crs$input == "+proj=longlat +datum=WGS84",
        msg = "The CRS of your bounding box is incorrect. Use the following code to get a bbox with the CRS OSM requires:\n\nsf::st_transform(sf_object, '+proj=longlat +datum=WGS84') %>% sf::st_bbox()\n\n"
    )



    # Set the OSM query connection
    sf_list <- bb %>%
        osmdata::opq(timeout = 50) %>%
        osmdata::add_osm_feature(
            key = feature_key,
            value = feature_values
        ) %>%
        osmdata::osmdata_sf(quiet = TRUE)

    return(sf_list)
}



#' Bind the results of an OpenStreeMap (OSM) feature query
#'
#' An OSM query returns a list of SFs of different types. This function binds
#' the ones you want together. If a requested geometry is NULL it will not be
#' returned, obviously. You need to use a CRS with latitude and longitude, not
#' decimal degrees.
#'
#' @param sf_list (\code{osmdata} object) as returned by
#'   \code{osm_query_features}
#' @param types (character vector) which SF types do you want. Valid options:
#'   lines, points, polygons, multilines, multipolygons
#' @param crs (numeric, default = 7844) which CRS should the result be?
#'   \code{osmdata} returns 4326 by default, but in order to intersect two sfs
#'   they must have the same crs, and if you are dealing with Australian
#'   geographies you should be using 7844.
#'
#' @return a \code{sf}
#' @export
#'
osm_bind <- function(sf_list,
                     types,
                     crs = 7844) {
    # Check the inputs
    assertthat::assert_that(inherits(sf_list, "osmdata"))
    assertthat::assert_that(all(types %in% c(
        "points",
        "lines",
        "polygons",
        "multilines",
        "multipolygons"
    )))


    # Add prefix to make names correct
    types_prefix <- stringr::str_c("osm_", types)

    # Filter sf_list to just what was requested
    kept_sf <- sf_list[types_prefix] %>%
        purrr::discard(.p = is.null)

    # Bind the final results
    if (length(kept_sf) == 1) {
        final_sf <- kept_sf[[1]]
    } else {
        final_sf <- dplyr::bind_rows(kept_sf)
    }

    # Break for invalid selection
    if (length(final_sf) == 0) {
        valid_geoms <- sf_list %>%
            purrr::discard(.p = is.null) %>%
            names() %>%
            purrr::keep(
                .p = stringr::str_starts,
                pattern = "osm_"
            ) %>%
            purrr::map(stringr::str_remove,
                pattern = "osm_"
            ) %>%
            paste(collapse = ", ")


        stop("The geometries you requested were all NULL.
         The valid choices for your sf_list are: ", valid_geoms)
    }

    final_sf <- final_sf %>%
        sf::st_make_valid() %>%
        sf::st_transform(crs = crs)

    return(final_sf)
}

#' Summarise OpenStreeMap (OSM) features by group
#'
#' Generally you will do this after querying the OSM features, binding the
#' results, intersecting with an irregular polygon (e.g. a burnscar).
#'
#' Requires the geometry column to be called geometry (and not geos or
#' some-such).
#'
#' @param osm_sf (\code{sf}) SF data you want to summarise.
#' @param ... (unquoted character; optional) columns to group by
#' @param .f (character) summary function. At the moment only "length", "area"
#'   or "count". Assumes the underlying data are additive.
#' @param units (character, default = NULL) units to summarise to, usually "km"
#'   or "km^2. If left blank it will use some variant of meters.
#'
#' @return a simple tibble with the grouped columns and an additional summary
#'   column
#' @export
#'
osm_summarise <- function(osm_sf,
                          ...,
                          .f,
                          units = NULL) {
    function_needs("osmdata") # nolint
    # Class check
    assertthat::assert_that(inherits(osm_sf, "sf"))
    assertthat::assert_that(length(.f) == 1)
    assertthat::assert_that(.f %in% c("length", "area", "count"))

    # Group by relevant columns
    if (!missing(...)) {
        osm_sf <- dplyr::group_by(osm_sf, ..., .add = TRUE)
    }

    # Use .f to build an expression (ie the code to be run)
    fun_raw <- paste0("sf::st_", .f, "(.data$geometry)")
    fun <- rlang::expr(!!fun_raw)

    if (.f %in% c("length", "area")) {
        # Run the summary function and assign to dynamic variable name
        summary_df <- osm_sf %>%
            dplyr::mutate(summary = eval(parse(text = fun))) %>%
            # Can't use strip_geomtery as it doesn't respect groups
            sf::st_set_geometry(NULL) %>%
            dplyr::summarise({{ .f }} := sum(summary))
    } else {
        summary_df <- osm_sf %>%
            # Can't use strip_geomtery as it doesn't respect groups
            sf::st_set_geometry(NULL) %>%
            dplyr::count(name = "count")
    }


    if (!is.null(units)) {
        units(summary_df[[.f]]) <- units
    }

    return(summary_df)
}
