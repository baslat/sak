# This script contains the code to create all internal datasets. Please don't
# change things unless you know what you're doing.

# Write the data items to system data
usethis::use_data(suggest_zoom_model, internal = TRUE, overwrite = TRUE)

# `geom_defaults` data ----

# Go to zzz.R and comment out set_plot_colours(type = "cah")

devtools::load_all()
check_subclass <- utils::getFromNamespace("check_subclass", "ggplot2")

# Library any packages that add geoms so you can take their defaults
# Check the geom names are in all_geoms in colours_and_plotting.R
library(ggrepel)

geom_defaults <- all_geoms %>% ## look at this is does what I want
    purrr::map(purrr::safely(check_subclass),
        "Geom",
        env = parent.frame()
    ) %>%
    purrr::map(
        purrr::pluck,
        "result"
    ) %>%
    purrr::map(
        purrr::pluck,
        "default_aes"
    ) %>%
    purrr::set_names(all_geoms)




# Uncomment the line in zzz.R
# Restart your R session


# Suggest zoom data ----
# Prepare model to suggest zoom

lat_m <- 111111
bp <- base_map_params %>%
    dplyr::mutate(
        zoom = dplyr::case_when(
            geo_name == "Perth" ~ 10,
            TRUE ~ zoom
        ),
        mean_lat = mean(top, bottom),
        lat_range = abs(top - bottom),
        lon_range = abs(right - left),
        vert = lat_range * lat_m,
        horz = abs(lon_range * cos(mean_lat) * lat_m),
        met = (cos(mean_lat * pi / 180) * 2 * pi * 6378137) / (256 * (2^zoom))
    )

suggest_zoom_model <- lm(met ~ 0 + vert + horz + I(vert^2) + I(horz^2), bp)

