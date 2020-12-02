test_that("censusnz::get_data() works for correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "SA1", variables = c("maori_descent", "smoking_status")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
  expect_silent(result <- censusnz::get_data(geography = "RC", variables = c("median_age_curp", "difficulty_hearing")))
  expect_silent(result <- censusnz::get_data("sa2", c("maori_descent", "sex")))
})
