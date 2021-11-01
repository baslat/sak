test_that("append_time works", {
  filename <- basename(
    append_time("test.csv",
      time = as.POSIXct(strptime("2010-10-31 01:30:00", "%Y-%m-%d %H:%M:%S"))
    )
  )

  expect_equal(
    filename,
    "test_2010_10_31_0130.csv"
  )
})
