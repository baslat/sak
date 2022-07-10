# This script contains the code to create all internal datasets. Please don't
# change things unless you know what you're doing.

# Write the data items to system data
usethis::use_data(suggest_zoom_model, internal = TRUE, overwrite = TRUE)

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
    horiz = abs(lon_range * cos(mean_lat) * lat_m),
    met = (cos(mean_lat * pi / 180) * 2 * pi * 6378137) / (256 * (2^zoom))
  )

suggest_zoom_model <- lm(met ~ 0 + vert + horiz + I(vert^2) + I(horiz^2), bp)
