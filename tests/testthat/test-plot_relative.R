test_that("plot_relative runs without error", {
  expect_silent(plot_relative(geography = "RC",
                              variables = "smoking_status",
                              n = 2))
  expect_silent(plot_relative(geography = "RC",
                              variables = "smoking_status",
                              n = 2,
                              regions = c("Auckland Region",
                                          "Northland Region")))
})
