betas <- function(train_predictors, broom_coefs) {
  browser()

  train_predictors

  predictors <- names(train_predictors)
  predictors_regex <- paste0(predictors, collapse = "|")

  base_df <- tibble(factor = "(intercept)", category = "(intercept)")

  for(pred in predictors) {
    x <- train_predictors[[pred]]
    x_coefs <- broom_coefs %>%
      dplyr::filter(stringr::str_detect(string = term, pattern = pred))



  }

}
