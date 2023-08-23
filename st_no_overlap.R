st_no_overlap <- function(polygons) {
	# TODO silence warning about constant attributes
	centroids <- sf::st_centroid(polygons)

	# Voronoi tesselation
	voronoi <- centroids |>
		sf::st_geometry() |>
		sf::st_union() |>
		# TODO convert to meters crs
		sf::st_voronoi() |>
		sf::st_collection_extract()

	# Put them back in their original order
	voronoi <- voronoi[unlist(sf::st_intersects(centroids, voronoi))]

	# Keep the attributes
	result <- centroids

	# Intersect voronoi zones with buffer zones
	sf::st_geometry(result) <-
		mapply(
			function(x, y) sf::st_intersection(x, y),
			# st_buffer(st_geometry(centroids),dist),
			polygons[["geometry"]],
			voronoi,
			SIMPLIFY = FALSE
		) |>
		sf::st_sfc(crs = sf::st_crs(centroids))

	result
}
