# This script contains the code to create all internal datasets. Please don't
# change things unless you know what you're doing.

# Write the data items to system data
usethis::use_data(suggest_zoom_model, internal = TRUE, overwrite = TRUE)

# Prepare model to suggest zoom

lat_m <- 111111L
bp <- base_map_params %>%
  dplyr::mutate(
    zoom = dplyr::case_when(
      geo_name == "Perth" ~ 10L,
      TRUE ~ zoom
    ),
    mean_lat = mean(top, bottom),
    lat_range = abs(top - bottom),
    lon_range = abs(right - left),
    vert = lat_range * lat_m,
    horiz = abs(lon_range * cos(mean_lat) * lat_m),
    met <- (cos(mean_lat * pi / 180L) * 2L * pi * 6378137L) / (256L * (2L^zoom))
  )

suggest_zoom_model <- lm(met ~ 0L + vert + horiz + I(vert^2L) + I(horiz^2L), bp)
