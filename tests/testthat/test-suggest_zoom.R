test_that("suggest_zoom works", {
  nc <- sf::st_read(
    system.file(
      "shape",
      "nc.shp",
      package = "sf"
    )
  )
  zoom <- suggest_zoom(nc)
  testthat::expect_identical(zoom, 7.0)
})
