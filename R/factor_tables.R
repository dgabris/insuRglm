factor_tables <- function(setup, betas, predictions) {
  train <- setup$data_train
  target <- setup$target
  weight <- setup$weight

  target_vector <- train[[target]]
  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  simple_factors <- setup$simple_factors
  predictors <- setup$current_model$predictors

  vars <- c(predictors, setdiff(simple_factors, predictors))
  factor_tables <- list()

  for(var in vars) {
    x <- train[[var]]

    if(inherits(x, "custom_factor") || inherits(x, "variate")) {
      mapping <- attr(x, "mapping")
    } else {
      orig_levels <- attr(x, "orig_levels")
      mapping <- setNames(orig_levels, orig_levels)
    }

    obs_avg <- compute_obs_avg(x, target_vector, weight_vector)
    fitted_avg <- compute_fitted_avg(x, predictions, weight_vector)

    x_betas <- betas %>% dplyr::filter(factor %in% c(var, "(Intercept)"))
    model_avg <- compute_model_avg(x, x_betas)

    one_table <- dplyr::bind_cols(
      factor = rep(var, length(mapping)),
      orig_level = names(mapping),
      actual_level = unname(mapping)) %>%
    dplyr::left_join(obs_avg, by = c("orig_level")) %>%
    dplyr::left_join(fitted_avg, by = c("orig_level")) %>%
    dplyr::left_join(model_avg, by = c("orig_level"))

    factor_tables[[var]] <- one_table
  }

  factor_tables

}
