test_that("normalise_geo_names works", {
  change_this <- tibble::tibble(lga_code_2018   = 1,
                                LGA_NAME_2018   = 1,
                                state_code_2016 = 1,
                                state_name_2016 = 1,
                                areasqkm_2018   = 1,
                                sa1code_2016    = 1,
                                sa1name_2016    = 1,
                                ucl_code18      = 1,
                                ucl_name18      = 1)

  norm_tt <- normalise_geo_names(change_this,
                                    remove_year = TRUE,
                                    make_lower = TRUE)


  expect_tt <-  tibble::tibble(lga_code   = "1",
                               lga_name   = "1",
                               state_code = "1",
                               state_name = "1",
                               areasqkm   = 1,
                               sa1code    = "1",
                               sa1name    = "1",
                               ucl_code   = "1",
                               ucl_name   = "1")

  expect_identical(norm_tt, expect_tt)


  norm_tf <- normalise_geo_names(change_this,
                                 remove_year = TRUE,
                                 make_lower = FALSE)


  expect_tf <-  tibble::tibble(lga_code   = "1",
                               LGA_NAME   = "1",
                               state_code = "1",
                               state_name = "1",
                               areasqkm   = 1,
                               sa1code    = "1",
                               sa1name    = "1",
                               ucl_code   = "1",
                               ucl_name   = "1")

  expect_identical(norm_tf, expect_tf)


  norm_ft <- normalise_geo_names(change_this,
                                 remove_year = FALSE,
                                 make_lower = TRUE)


  expect_ft <-  tibble::tibble(lga_code_2018   = "1",
                               lga_name_2018   = "1",
                               state_code_2016 = "1",
                               state_name_2016 = "1",
                               areasqkm_2018   = 1,
                               sa1code_2016    = "1",
                               sa1name_2016    = "1",
                               ucl_code18      = "1",
                               ucl_name18      = "1")

  expect_identical(norm_ft, expect_ft)



  norm_ff <- normalise_geo_names(change_this,
                                 remove_year = FALSE,
                                 make_lower = FALSE)


  expect_ff <-  tibble::tibble(lga_code_2018   = "1",
                               LGA_NAME_2018   = "1",
                               state_code_2016 = "1",
                               state_name_2016 = "1",
                               areasqkm_2018   = 1,
                               sa1code_2016    = "1",
                               sa1name_2016    = "1",
                               ucl_code18      = "1",
                               ucl_name18      = "1")

  expect_identical(norm_ff, expect_ff)





})
