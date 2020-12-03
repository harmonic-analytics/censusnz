test_that("get_data() works for correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "RC", variables = c("maori_descent", "smoking_status")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
})

test_that("get_data() errors on missing inputs", {
  expect_error(censusnz::get_data(variables = c("maori_descent", "smoking_status")), "Must provide a geography")
  expect_error(censusnz::get_data(geography = "RC"), "Must provide a variable")
})

test_that("get_data() errors correctly on incorrect inputs", {
  expect_silent(censusnz::get_data(geography = "RC", variables = c("maorii_descent", "smoking_status")))
  expect_error(censusnz::get_data(geography = "RC", variables = c("maorii_descent", "smokiing_status")))
  expect_error(censusnz::get_data(geography = "RRCC", variables = c("maori_descent", "smoking_status")))
})

test_that("get_data() works on incorrectly capitalised correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "rc", variables = c("mAoRI_dEsCenT", "smoking_STATUS")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("geoid", "land_type", "name", "variable", "variable_group", "count") %in% colnames(result)))
})
