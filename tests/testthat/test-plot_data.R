test_that("plot_data runs without error", {
  expect_silent(plot_data(geography = "RC",
                              variables = "smoking_status",
                              n = 2))
})
