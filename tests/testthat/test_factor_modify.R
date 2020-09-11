setup <- sev_setup %>%
  factor_add(pol_yr)


context("Model Formula - modify")

test_that("first argument must be of class setup", {

  expect_error(
    factor_modify(unclass(setup), pol_yr = custom_factor(pol_yr, mapping = c(1, 1, 2, 3, 4))),
    "Setup object is not correct"
  )

})

test_that("provided expression must be named", {

  expect_error(
    factor_modify(setup, custom_factor(pol_yr, mapping = c(1, 1, 2, 3, 4))),
    "Please provide a named expression"
  )

})

test_that("expression can modify an existing (potential) predictor only", {

  expect_error(
    factor_modify(setup, new_column = custom_factor(pol_yr, mapping = c(1, 1, 2, 3, 4))),
    "Please use the original predictor name as the name of argument"
  )

})

test_that("factor_modify produces what it should in both train and test datasets", {

  setup <- setup %>%
    factor_modify(pol_yr = custom_factor(pol_yr, mapping = c(1, 1, 2, 3, 4)))

  train_vector <- setup$data_train$pol_yr
  test_vector <- setup$data_test$pol_yr

  expect_equal(class(train_vector), c("custom_factor", "simple_factor", "factor"))
  expect_equal(attr(train_vector, "mapping"), setNames(c(1, 1, 2, 3, 4), 2000:2004))

  expect_equal(class(test_vector), c("custom_factor", "simple_factor", "factor"))
  expect_equal(attr(test_vector, "mapping"), setNames(c(1, 1, 2, 3, 4), 2000:2004))

})
