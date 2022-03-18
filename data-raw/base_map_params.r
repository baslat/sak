## code to prepare `base_map_params` dataset goes here
library(sf)
ucl <- absmapsdata::ucl2016 %>%
    normalise_geo_names()

# Prep ----

# ucl capital cities ----
melb <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Melbourne",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Melbourne") %>% sf::st_bbox() %>% list(),
    zoom = 11
)



adelaide <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Adelaide",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Adelaide") %>% sf::st_bbox() %>% list(),
    zoom = 11
)

brisbane <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Brisbane",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Brisbane") %>% sf::st_bbox() %>% list(),
    zoom = 11
)
hobart <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Hobart",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Hobart") %>% sf::st_bbox() %>% list(),
    zoom = 12
)
darwin <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Darwin",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Darwin") %>% sf::st_bbox() %>% list(),
    zoom = 13
)


cbr <- osmdata::getbb("Canberra, Australia")
cbr_bb <- c(
    xmin = cbr[1, 1],
    ymin = cbr[2, 1],
    xmax = cbr[1, 2],
    ymax = cbr[2, 2])

canberra <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Canberra",
    bbox = cbr_bb %>% list(),
    zoom = 12
)



perth <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Perth",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Perth (WA)") %>% sf::st_bbox() %>% list(),
    zoom = 11
)
sydney <- tibble::tibble(
    geo_level = "ucl",
    geo_name = "Sydney",
    bbox = dplyr::filter(ucl, .data$ucl_name == "Sydney") %>% sf::st_bbox() %>% list(),
    zoom = 11
)

# States ----
nsw <- tibble::tibble(
    geo_level = "state",
    geo_name = "NSW",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "New South Wales" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 7
)
vic <- tibble::tibble(
    geo_level = "state",
    geo_name = "Vic",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "Victoria" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 8
)

sa <- tibble::tibble(
    geo_level = "state",
    geo_name = "SA",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "South Australia" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 7
)

qld <- tibble::tibble(
    geo_level = "state",
    geo_name = "Qld",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "Queensland" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 7
)

wa <- tibble::tibble(
    geo_level = "state",
    geo_name = "WA",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "Western Australia" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 7
)

tas <- tibble::tibble(
    geo_level = "state",
    geo_name = "Tas",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "Tasmania" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 8
)

nt <- tibble::tibble(
    geo_level = "state",
    geo_name = "NT",
    bbox = dplyr::filter(
        ucl,
        .data$state_name == "Northern Territory" &
            .data$sos_name == "Rural Balance"
    ) %>%
        sf::st_bbox() %>%
        list(),
    zoom = 7
)


# Mapping -----
base_map_params <- dplyr::bind_rows(
    melb,
    wa,
    brisbane,
    hobart,
    darwin,
    # canberra,
    sydney,
    perth,
    nsw,
    vic,
    sa,
    qld,
    tas,
    nt,
    adelaide
) %>%
    # slice(6:7) %>%
    tidyr::unnest_wider(bbox) %>%
    dplyr::transmute(geo_level,
        geo_name,
        left = xmin,
        bottom = ymin,
        right = xmax,
        top = ymax,
        zoom,
        maptype = "toner-lite"
    ) %>%
    dplyr::arrange(desc(geo_level), geo_name) %>%
    dplyr::rowwise()


usethis::use_data(base_map_params, overwrite = TRUE)
