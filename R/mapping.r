#' Extract and optionally expand a bounding box
#'
#' Extract the bounding box from an object and optionally expand it. This can
#' take an \code{sf} or a \code{tibble} with lat and lon columns.
#'
#' @param .data (\code{tibble}) a \code{tibble} or \code{sf}
#' @param exp_factor (numeric; default = 0.1) the expansion factor
#' @param lon (character \[optional\]; default = 'lon') name of longitude column
#'   if a data.frame
#' @param lat (character \[optional\]; default = 'lat') name of latitude column if
#'   a data.frame
#' @param crs (numeric \[optional\]; default = 7844) CRS if converting data.frame
#'   to sf
#'
#' @return A named bounding box vector with names "left", "bottom", "right" and
#'   "top".
#' @export
#'
#'

expand_bbox <- function(.data,
                        exp_factor = 0.1,
                        lon = "lon",
                        lat = "lat",
                        crs = 7844) {

    # If not sf, turn into sf
    if (!any(stringr::str_detect(class(.data), "sf"))) {
        .data <- .data %>%
            sf::st_as_sf(coords = c(lon, lat), crs = crs)
    }
    # Get the bounding box
    bb <- .data %>%
        sf::st_bbox()

    # Get the range of the data passed
    xrange <- bb$xmax - bb$xmin
    yrange <- bb$ymax - bb$ymin

    # Replace values with expanded values
    bb[1] <- bb[1] - (exp_factor * xrange) # xmin - left
    bb[2] <- bb[2] - (exp_factor * yrange) # ymin - bottom
    bb[3] <- bb[3] + (exp_factor * xrange) # xmax - right
    bb[4] <- bb[4] + (exp_factor * yrange) # ymax - top

    # Need to add a name switch, but when is it xmin and when is it left?
    names(bb) <- c(
        "left",
        "bottom",
        "right",
        "top"
    )

    return(bb)
}



#' Extract the aspect ratio of a ggplot map
#'
#' Take a plotted \code{ggmap} object and extract its aspect ratio.
#'
#' @param .ggmap (ggmap) a map
#'
#' @return (numeric) the aspect ratio of height to width
#' @export
#' @examples
#' \dontrun{
#' map_raw <- get_stamenmap()
#' map <- ggmap(map_raw)
#' asp <- extract_map_aspect_ratio(map)
#' }
#'
extract_map_aspect_ratio <- function(.ggmap) {
    function_needs("ggmap")
    # Get the coordinate projection
    coord <- ggplot2::coord_quickmap(.ggmap)

    # Get the ranges
    x_range <- ggplot2::ggplot_build(.ggmap)$layout$panel_scales_x[[1]]$range$range
    y_range <- ggplot2::ggplot_build(.ggmap)$layout$panel_scales_y[[1]]$range$range


    # Calculate the aspect ratio of height to width
    asp <- coord$aspect(list(x.range = x_range, y.range = y_range))

    return(asp)
}


#' Normalise ABS geography names
#'
#' This function renames and reclassifies the columns of a `data.frame` to make
#' them easier to work with. It will always reclassify any column with
#' \code{code} in the name or with some sort of year identifier (e.g.
#' \code{lga_name_2018}) to \code{character}. Optionally, it can remove these
#' year identifiers from the column names and/or rename the columns to lower
#' case.
#'
#'
#' @param .data (\code{tibble}) A df with columns you want renamed
#' @param remove_year (logical; default = TRUE) Do you want to remove year
#'   identifies in the column names?
#' @param make_lower (boolean; default = TRUE) Do you want to set all column
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
    function_needs("tidyselect")

    clean_dat <- .data %>%
        # Make columns characters, and exclude columns with area in the name
        dplyr::mutate(dplyr::across(
            c(
                tidyselect::matches("(_[0-9]{2,4}|[0-9]{2,4}|code)"),
                -tidyselect::matches("area")
            ), # Not the area column
            as.character
        ))

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
            dplyr::rename_with(
                .fn = tolower,
                .cols = tidyselect::everything()
            )
    }
    clean_dat
}


#' @rdname normalise_geo_names
#' @export
standardise_geo_names <- normalise_geo_names

#' Fix ggmap bounding box
#'
#' The function is part of a process to fix an issue in \code{ggplot2} where
#' vector \code{sf} layers and raster \code{ggmap} layers do not align properly.
#' It renames the bounding box element to the names \code{st_bbox} expects,
#' converts the bbox to an \code{sf} polygon, transforms it to EPSG:3875, and
#' then converts back to a bbox.
#'
#' @param .map (ggmap) A ggmap object as returned by \code{get_stamenmap} or
#'   similar
#'
#' @return returns the original ggmap object but with a respecified bounding box
#'   using EPSG:3857 (WGS84)
#' @export
#'
#' @examples
#' \dontrun{
#' actmap <- get_stamenmap(act_bbox, maptype = "toner-lite", zoom = 13)
#' actmap <- fix_ggmap_bbox(actmap)
#' map_data <- st_transform(map_data, crs = 3857)
#' map_data <- mutate(map_data, geometry = st_transform(geometry, crs = 3857))()
#' }
#'
fix_ggmap_bbox <- function(.map) {
    function_needs("ggmap")
    assertthat::assert_that(inherits(.map, "ggmap"),
        msg = ".map must be a ggmap object"
    )

    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- stats::setNames(
        unlist(attr(.map, "bb")),
        c("ymin", "xmin", "ymax", "xmax")
    )

    # Convert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- sf::st_bbox(map_bbox, crs = 4326) %>%
        sf::st_as_sfc() %>%
        sf::st_transform(3857) %>%
        sf::st_bbox()

    # nolint start
    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(.map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(.map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(.map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(.map, "bb")$ur.lon <- bbox_3857["xmax"]
    # nolint end
    return(.map)
}


#' Strip geometry column
#'
#' Turns an \code{sf} object into a \code{tibble} and removes the geometry
#' column.
#'
#' This function improves on the \code{sf_drop_geometry()} in \code{sf} by
#' turning the object into a \code{tibble}, rather than a \code{data.frame}.
#'
#' @param .sf an \code{sf} object
#' @inheritParams tibble::tibble
#' @return The original \code{sf} as a \code{tibble} and without the geometry
#'   column.
#' @export
#'
#' @examples
#' \dontrun{
#' sf <- tibble::tibble(
#'     incident_type = c("fire", "flood", "cyclone"),
#'     `Date start` = c("2020-08-13", "2020-09-17", "2020-10-24"),
#'     `Responsible officials` = c("Tom", "Bob", "Jenny"),
#'     lat = c(38.66, 18.3, 25.33),
#'     lon = c(143.3, 132.7, 140.2)
#' ) %>%
#'     sf::st_as_sf(
#'         coords = c("lon", "lat"),
#'         crs = "+proj=longlat +datum=WGS84"
#'     )
#'
#' strip_geometry(.sf = sf, .name_repair = "universal")
#'
#' strip_geometry(.sf = sf, .name_repair = "minimal")
#'
#' strip_geometry(.sf = sf, .name_repair = "check_unique")
#'
#' strip_geometry(.sf = sf, .name_repair = janitor::make_clean_names)
#' }
strip_geometry <- function(.sf,
                           .name_repair = "universal") {
    assertthat::assert_that(class(.sf)[1] == "sf",
        msg = "Object is not of class sf"
    )
    assertthat::assert_that(is.character(.name_repair) | is.function(.name_repair),
        msg = ".name_repair is not a character or a function which acts on the column names."
    )
    assertthat::assert_that(length(.name_repair) == 1,
        msg = ".name_repair is not of length 1, please enter a single quoted character or function not a list."
    )
    assertthat::assert_that(if (is.character(.name_repair)) {
        .name_repair %in% c("minimal", "unique", "check_unique", "universal")
        TRUE
    },
    msg = glue::glue(".name_repair is not one of 'minimal', 'unique', 'check_unique', 'universal'")
    )

    .sf %>%
        sf::st_set_geometry(NULL) %>%
        tibble::as_tibble(.name_repair = .name_repair)
}


#' A faster st_intersection
#'
#' This function is a faster way to perform \code{sf::st_intersection()}. It
#' only calculates the intersection for those geometries that actually
#' intersect.
#'
#' @param .x an sf object
#' @param .y an sf object
#' @param verbose (boolean; default = \code{TRUE}) do you want to be informed of
#'   the progress?
#'
#' @return an sf object
#' @export
#'
#' @examples
#' \dontrun{
#' st_intersection_quicker(burnscar, lga_shapefile)
#' }
st_intersection_quicker <- function(.x,
                                    .y,
                                    verbose = TRUE) {
    old_crs_x <- sf::st_crs(.x)
    old_crs_y <- sf::st_crs(.y)

    assertthat::assert_that(old_crs_x == old_crs_y,
        msg = "The CRS of each sf has to be the same"
    )

    # Silence some warnings about
    # "attribute variables are assumed to be spatially constant throughout all geometries"
    sf::st_agr(.x) <- "constant"
    sf::st_agr(.y) <- "constant"

    # Convert to equi-rectangular projection to silence planar warnings
    .x <- sf::st_transform(.x, "+proj=eqc")
    .y <- sf::st_transform(.y, "+proj=eqc")

    # Get just the features that intersect
    common <- sf::st_intersects(.x, .y)

    feat_subset <- .x[lengths(common) > 0, ]

    if (verbose) {
        # Estimate time and provide some messages so the user can read something while
        # they wait
        n_rows <- nrow(feat_subset)
        per_row <- 1000 / 74

        new_seconds <- per_row * n_rows / 1e3
        old_seconds <- per_row * nrow(.x) / 1e3

        time_new <- lubridate::seconds_to_period(new_seconds)
        time_new_form <- sprintf(
            "%02d:%02d:%02d",
            time_new@hour,
            lubridate::minute(time_new),
            round(lubridate::second(time_new))
        )

        time_old <- lubridate::seconds_to_period(old_seconds)
        time_old_form <- sprintf(
            "%02d:%02d:%02d",
            time_old@hour,
            lubridate::minute(time_old),
            round(lubridate::second(time_old))
        )

        time_save <- lubridate::seconds_to_period(old_seconds - new_seconds)
        time_save_form <- sprintf(
            "%02d:%02d:%02d",
            time_save@hour,
            lubridate::minute(time_save),
            round(lubridate::second(time_save))
        )


        glue::glue("{scales::label_comma()(n_rows)} intersecting features, this might take around {time_new_form}.") %>%
            message()


        glue::glue("If you used `sf::st_intersection()`, this would have taken about {time_old_form}.") %>%
            message()

        glue::glue("So, this new function is about {scales::label_percent()(1 - new_seconds/old_seconds)} faster, and saved you about {time_save_form}. What will you do with all your new time?") %>%
            message()
    }
    # Calculate the intersection on the subset and restore the CRS
    intersected_data <- sf::st_intersection(feat_subset, .y) %>%
        sf::st_transform(old_crs_x)

    return(intersected_data)
}
utils::globalVariables(c("time_new_form", "time_old_form", "time_save_form"))


# Map data ----


#' Parameters for UCL and state base maps
#'
#' A \strong{rowwise} \code{tibble} containing the zoom level and bounding box
#' details for UCL and state geographies. Used to functionalise the downloading
#' of base maps. It's rowwise for use with \code{embed_gmap}.
#'
#' @format A \code{tibble} with the following columns:
#' \describe{
#' \item{geo_level}{the level of geography, such as UCL or state}
#' \item{geo_name}{the name of the geography, such as Adelaide or NSW}
#' \item{left}{part of the bounding box}
#' \item{bottom}{part of the bounding box}
#' \item{right}{part of the bounding box}
#' \item{top}{part of the bounding box}
#' \item{zoom}{the preferred zoom of the base map}
#' \item{maptype}{the preferred base map type}
#' }
#'
#' @source ABS for the UCL shapefile from which the bounding boxes are extracted
"base_map_params"

#' Get and embed a \code{ggmap} in a tibble
#'
#' \code{ggmap} objects won't nicely go into a cell in a \code{tibble}. This
#' function gets a \code{ggmap} and then wraps it in a list, allowing it to go
#' into a \code{tibble}.
#'
#'
#'
#' @param left (numeric) part of the bounding box
#' @param bottom (numeric) part of the bounding box
#' @param right (numeric) part of the bounding box
#' @param top (numeric) part of the bounding box
#' @param zoom (numeric) the zoom level
#' @param maptype (character) the map type
#' @param ... other arguments passed to \code{ggmap::get_stamen_map}
#'
#' @return a \code{ggmap} object wrapped in a list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' embedded_maps <- base_map_params %>%
#'     filter(geo_level == "state") %>%
#'     mutate(gmap = embed_gmap(left, bottom, right, top, zoom, maptype)) %>%
#'     ungroup()
#' }
embed_gmap <- function(left, bottom, right, top,
                       zoom, maptype, ...) {
    function_needs("ggmap")
    ggmap::get_stamenmap(
        bbox = c(left, bottom, right, top),
        zoom = zoom,
        maptype = maptype,
        ...
    ) %>%
        list()
}


#' Extract the \code{ggmap}s from a \code{tibble} column
#'
#' @param .data (\code{tibble}) the result of a pipe with \code{embed_gmap}
#' @param col (unquoted character) the column with the embedded maps
#'
#' @return a named \code{list} of \code{ggmap} base maps
#' @export
#'
#' @examples
#' \dontrun{
#' embedded_maps <- base_map_params %>%
#'     filter(geo_level == "state") %>%
#'     mutate(gmap = embed_gmap(left, bottom, right, top, zoom, maptype)) %>%
#'     ungroup()
#'
#' list_of_basemaps <- extract_gmaps(embedded_maps) %>% map(fix_ggmap_bbox)
#' }
extract_gmaps <- function(.data,
                          col) {
    map_names <- .data %>%
        dplyr::ungroup() %>%
        dplyr::pull(.data$geo_name)

    extracted_maps <- .data %>%
        dplyr::ungroup() %>%
        dplyr::pull({{ col }}) %>%
        purrr::set_names(map_names)

    # If the incoming tibble is one row, I expect the result to be a gmap, not a
    # list of length one
    if (length(extracted_maps) == 1) {
        extracted_maps <- extracted_maps[[1]]
    }

    return(extracted_maps)
}

#' A model output to predict meters per pixel given coordinates
#'
#' A model output to predict how many meters a pixel represents, given a
#' bounding box's vertical and horizontal distance. The predicted meters are
#' then used in \code{suggest_zoom} (along with other data) to suggest a zoom
#' level for a static map.
#'
#' @format A \code{lm} object that requires vertical and horizontal distance
#'   (and their squares) in meters.
#'
#' @source CAH calculations based on \code{base_map_params}
"suggest_zoom_model"


#' Suggest a zoom level for a basemap
#'
#' The curse of Australian geography are the areas span orders of magnitudes.
#' This means automating maps with basemaps is troublesome because a suitable
#' zoom for one area is wrong for another. Well, no more! This function uses
#' some sweet trigonometry and logarithms to suggest an appropriate zoom level.
#'
#' @param bbox a word string bounding box (ie with labels 'left', 'top' etc)
#'
#' @return a suggested zoom level as a number
#' @export
#'
#' @examples
#' \dontrun{
#' act <- c("left" = 148.936148, "bottom" = -35.547871, "right" = 149.268683, "top" = -35.119909)
#' attr(act, "class") <- "bbox"
#' suggest_zoom(bbox)
#' }
suggest_zoom <- function(bbox) {
    # Get some averages of the bbox
    mean_lat <- mean(c(bbox["top"], bbox["bottom"]))
    lat_range <- unname(abs(bbox["top"] - bbox["bottom"]))
    lon_range <- unname(abs(bbox["right"] - bbox["left"]))

    # Rough distance of one degree (m)
    lat_m <- 111111

    # Distance of bounding box in m
    vert <- lat_range * lat_m
    horz <- abs(lon_range * cos(mean_lat) * lat_m)

    # Model how many meters each pixel should show
    met <- unname(stats::predict(suggest_zoom_model, tibble::tibble(vert, horz)))

    zoom <- round(log2((cos(mean_lat * pi / 180) * pi * 6378137) / (128 * met)))
    # 18 is the max
    zoom <- min(zoom, 18)

    return(zoom)
}

#' Swap the coordinates in an `sf`
#'
#' Sometimes people put coordinates in back to front. This function swaps them.
#'
#' @param .data (`sf`) an `sf` object
#' @param geometry (unquoted character; default = geometry) the geometry column
#'
#' @export
#'
#' @examples
#' /dontrun{
#' sf %>% swap_coords()
#' }
swap_coords <- function(.data, geometry = geometry) {
    .data %>%
        dplyr::mutate({{ geometry }} := purrr::modify(
            {{ geometry }},
            purrr::modify,
            ~ list(.[[1]][, c(2, 1)])
        ))
}
