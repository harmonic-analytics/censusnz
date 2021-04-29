test_that("get_variables() works with no inputs", {
  expect_silent(variables <- censusnz::get_variables())
  expect_true("data.frame" %in% class(variables))
  expect_setequal(colnames(variables), c("geography", "variable", "category", "subcategory", "concept"))
  expect_gt(nrow(variables), 0)
})
