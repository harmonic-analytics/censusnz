test_that("censusnz::get_variables() works", {
  expect_silent(variables <- censusnz::get_variables())
  expect_true("data.frame" %in% class(variables))
  expect_setequal(colnames(variables), c("geography", "variable", "category", "subcategory", "concept"))
  expect_gt(nrow(variables), 0)
})

test_that("censusnz::get_data() works", {
  expect_silent(result <- censusnz::get_data(geography = "SA1", variables = c("maori_descent", "smoking_status")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
  expect_silent(result <- censusnz::get_data(geography = "RC", variables = c("median_age_curp", "difficulty_hearing")))
  expect_silent(result <- censusnz::get_data("sa2", c("maori_descent", "sex")))
})


