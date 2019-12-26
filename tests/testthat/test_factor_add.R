require(dplyr) # for the pipe operator
data('sev_train')

no_print <- function(expr) {
  invisible(capture.output(expr))
}

args <- list(
  data_train = sev_train,
  target = 'sev',
  weight = 'numclaims',
  family = 'gamma',
  keep_cols = c('pol_nbr', 'exposure', 'premium')
)

no_print(setup <- do.call(setup, args))


context("Model Formula - add")

test_that("first argument must be of class setup", {

  expect_error(factor_add(unclass(setup), pol_yr), "Setup object is not correct")

})

test_that("factor to  be added must be present in the datasets", {

  expect_error(factor_add(setup, imaginary_column), "This predictor is not present in the dataset provided to setup object.")

})

test_that("the same factor can't be added multiple times", {

  setup <- setup %>%
    factor_add(pol_yr)

  expect_message(setup <- factor_add(setup, pol_yr), "Can't add 'pol_yr'. It's already among predictors.")

  expect_equal(setup$current_model$predictors, c("pol_yr"))

})

test_that("factor_add produces what it should", {

  setup <- setup %>%
    factor_add(pol_yr) %>%
    factor_add(agecat)

  expect_equal(setup$current_model$predictors, c("pol_yr", "agecat"))

})
