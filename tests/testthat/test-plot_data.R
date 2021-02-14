test_that("plot_data runs without error", {
  expect_silent(plot_data(geography = "RC",
                          variables = "smoking_status",
                          n = 2))
  expect_silent(plot_data(geography = "RC",
                          variables = "smoking_status",
                          regions = "Auckland Region",
                          n = 2))
})

test_that("plot_data returns ggplot object", {
  my_plot = plot_data(geography = "WARD",
                      variables = "sex")
  expect_is(my_plot, "ggplot")
})
