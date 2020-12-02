test_that("get_data() works for correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "SA1", variables = c("maori_descent", "smoking_status")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
  expect_silent(result <- censusnz::get_data(geography = "RC", variables = c("median_age_curp", "difficulty_hearing")))
  expect_silent(result <- censusnz::get_data("sa2", c("maori_descent", "sex")))
})

test_that("get_data() errors on missing inputs", {
  expect_error(censusnz::get_data(variables = c("maori_descent", "smoking_status")), "Must provide a geography")
  expect_error(censusnz::get_data(geography = "SA1"), "Must provide a variable")
})

test_that("get_data() errors correctly on incorrect inputs", {
  expect_silent(censusnz::get_data(geography = "SA1", variables = c("maorii_descent", "smoking_status")))
  expect_error(censusnz::get_data(geography = "SA1", variables = c("maorii_descent", "smokiing_status")))
  expect_error(censusnz::get_data(geography = "SAA1", variables = c("maori_descent", "smoking_status")))
})

test_that("get_data() works on incorrectly capitalised correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "sA1", variables = c("mAoRI_dEsCenT", "smoking_STATUS")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
})
