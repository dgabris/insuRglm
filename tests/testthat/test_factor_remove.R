setup <- sev_setup

context("Model Formula - remove")

test_that("first argument must be of class setup", {

  setup <- setup %>%
    factor_add(pol_yr)

  expect_error(factor_remove(unclass(setup), pol_yr), "Setup object is not correct")

})

test_that("factor to be removed must be present in the model formula", {

  setup <- setup %>%
    factor_add(pol_yr)

  expect_message(factor_remove(setup, agecat), "Can't remove 'agecat'. It's not among predictors.")

})

test_that("factor_remove produces what it should", {

  setup <- setup %>%
    factor_add(pol_yr) %>%
    factor_add(agecat) %>%
    factor_remove(agecat)

  expect_equal(setup$current_model$predictors, c("pol_yr"))

})

