test_that("get_data() works for correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "RC", variables = c("maori_descent", "smoking_status"), year = 2006))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("category", "year", "geography_type", "geoid", "land_type",  "name", "variable", "variable_group", "count") %in% colnames(result)))
  expect_silent(result <- censusnz::get_data("DHB", "maori_descent", 2013))

  # returns all variables expected, including those from all categories
  variables_in <- c("weekly_rent", "number_of_rooms", "hours_worked_per_week")
  expect_silent(result <- censusnz::get_data(geography = "SA2", variables = variables_in, year = 2006))
  expect_setequal(unique(result$variable), variables_in)
  expect_setequal(unique(result$category), c("Individual", "Dwelling", "Household"))

  # Can handle multiple years
  expect_silent(result <- censusnz::get_data(geography = "WARD", variables = c("smoking_status", "weekly_rent"), year = c(2013, 2018, 2006)))
  expect_setequal(unique(result$year), c(2013, 2018, 2006))
  expect_setequal(unique(result$variable), c("smoking_status", "weekly_rent"))
  expect_equal(nrow(result), 13284)
  expect_equal(length(result$year[result$year == 2006]), 4428)
  expect_equal(length(result$year[result$year == 2013]), 4428)
  expect_equal(length(result$year[result$year == 2018]), 4428)
  # No duplicate results in result returned
  expect_equal(nrow(result), nrow(result %>% dplyr::distinct()))
})

test_that("get_data() errors on missing geography and variable inputs", {
  expect_error(censusnz::get_data(variables = c("maori_descent", "smoking_status")))
  expect_error(censusnz::get_data(geography = "RC"))
})

test_that("get_data() errors correctly on incorrect inputs", {
  expect_error(censusnz::get_data(geography = "RC", variables = c("maorii_descent", "smoking_status")))
  expect_error(censusnz::get_data(geography = "RC", variables = c("maorii_descent", "smokiing_status")))
  expect_error(censusnz::get_data(geography = "RRCC", variables = c("maori_descent", "smoking_status")))
  expect_error(censusnz::get_data(geography = c("RC", "SA2"), variables = c("maori_descent", "smoking_status")))
})

test_that("get_data() works on incorrectly capitalised correct inputs", {
  expect_silent(result <- censusnz::get_data(geography = "rc", variables = c("mAoRI_dEsCenT", "smoking_STATUS")))
  expect_true("data.frame" %in% class(result))
  expect_true(all(c("category", "year", "geography_type", "geoid", "land_type",  "name", "variable", "variable_group", "count") %in% colnames(result)))
})

test_that("get_data() provides handles returning data where varaibales are not available", {
  expect_warning(result <- censusnz::get_data(geography = "TA", variables = c("dwelling_dampness_indicator", "smoking_status"), year = 2013))
  expect_setequal(unique(result$variable), c("smoking_status"))
  expect_warning(result <- censusnz::get_data(geography = "TA", variables = "dwelling_dampness_indicator", year = 2013))
  expect_equal(nrow(result), 0)
})
