test_that("suggest_zoom works", {
	nc <- sf::st_read(
		system.file(
			"shape",
			"nc.shp",
			package = "sf"
		)
	)
	bbox <- expand_bbox(nc)
	zoom <- suggest_zoom(bbox)
	testthat::expect_identical(zoom, 8.0)
})
