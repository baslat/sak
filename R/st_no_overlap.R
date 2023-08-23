#' Non-overlapping polygons using Voronoi tesselation
#'
#' This function takes a set of polygons and returns a new set of polygons where
#' the buffer zones of the original polygons do not overlap with each other.
#' This is achieved by calculating the centroids of the polygons, creating a
#' Voronoi tesselation around the centroids, and intersecting the Voronoi zones
#' with the buffer zones of the original polygons. The resulting polygons do not
#' overlap with each other, ensuring that they can be used for further analysis
#' or visualization.
#'
#' @param polygons A set of polygons with attributes.
#'
#' @return A set of polygons with attributes where the buffer zones of the original polygons do not overlap with each other.
#'
#' @details This function uses the `sf` package to calculate the centroids of
#' the polygons using the `st_centroid` function, and to create a Voronoi
#' tesselation around the centroids using the `st_voronoi` function. The Voronoi
#' zones are then intersected with the buffer zones of the original polygons
#' using the `st_intersection` function. The resulting polygons are returned
#' with their original attributes.
#'
#' @export
st_no_overlap <- function(polygons) {
 # silence warning about constant attributes
 sf::st_agr(polygons) <- "constant"
 orig_crs <- sf::st_crs(polygons)
	centroids <- sf::st_centroid(polygons)

 # Voronoi tesselation
	voronoi_raw <- centroids |>
		sf::st_geometry() |>
		sf::st_union() |>
		# convert to meters crs
		sf::st_transform(crs = 3857L) |>
		sf::st_voronoi() |>
		# convert back to original crs
		sf::st_transform(crs = orig_crs) |>
		sf::st_collection_extract()

 # Put them back in their original order
 voronoi <- voronoi_raw[unlist(sf::st_intersects(centroids, voronoi_raw))]

	# Keep the attributes
	result <- centroids

	# Intersect voronoi zones with buffer zones
 sf::st_geometry(result) <- purrr::map2(
		polygons[["geometry"]],
		voronoi,
		sf::st_intersection
	) |>
  sf::st_sfc(crs = orig_crs)

result

}
