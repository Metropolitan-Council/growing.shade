testthat::test_that("bg_growingshade_main data is valid", {
  test_bg_dat <- bg_growingshade_main %>%
    dplyr::filter(variable %in% c(
      "canopy_percent",
      "ndvi_land",
      "avg_temp"
    )) %>%
    dplyr::select(-name, -weights_scaled) %>%
    tidyr::pivot_wider(names_from = variable, values_from = raw_value)

  testthat::expect_equal(nrow(test_bg_dat), 2586)

  testthat::expect_equal(
    summary(test_bg_dat$canopy_percent),
    structure(c(
      Min. = 0, `1st Qu.` = 0.276528678117521, Median = 0.36926069583499,
      Mean = 0.361879065478218, `3rd Qu.` = 0.45971699843831, Max. = 0.737490080628285,
      `NA's` = 18
    ), class = c("summaryDefault", "table"))
  )

  testthat::expect_equal(
    summary(test_bg_dat$ndvi_land),
    structure(c(
      Min. = 0.141759841394238, `1st Qu.` = 0.502935458501074,
      Median = 0.569917376971841, Mean = 0.555199568665158, `3rd Qu.` = 0.625447677922254,
      Max. = 0.78710051009344, `NA's` = 18
    ), class = c(
      "summaryDefault",
      "table"
    ))
  )


  testthat::expect_equal(
    summary(test_bg_dat$avg_temp),
    structure(c(
      Min. = 86.5697729190336, `1st Qu.` = 92.160520595292,
      Median = 93.364045659604, Mean = 93.529167858947, `3rd Qu.` = 94.618962079741,
      Max. = 101.389420545641
    ), class = c("summaryDefault", "table"))
  )
})
