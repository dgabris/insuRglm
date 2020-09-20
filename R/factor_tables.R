factor_tables <- function(setup, betas, current_baseline, predictions) {
  train <- setup$data_train
  target <- setup$target
  weight <- setup$weight

  target_vector <- train[[target]]
  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  simple_factors <- setup$simple_factors
  predictors <- setup$current_model$predictors

  vars <- c(predictors, setdiff(simple_factors, predictors))
  factor_tables <- list()

  for(i in seq_along(vars)) {

    var <- vars[[i]]
    x <- train[[var]]

    if(inherits(x, "custom_factor") || inherits(x, "interaction") ||
       inherits(x, "offset")) {
      mapping <- attr(x, "mapping")

    } else if(inherits(x, "variate")) {
      mapping_df <- attr(x, "mapping")
      mapping <- setNames(mapping_df$actual_level, mapping_df$orig_level)

    } else {
      orig_levels <- attr(x, "orig_levels")
      mapping <- setNames(orig_levels, orig_levels)
    }

    obs_avg <- compute_obs_avg(x, target_vector, weight_vector)
    fitted_avg <- compute_fitted_avg(x, predictions, weight_vector)

    if(inherits(x, "interaction")) {
      interaction_var <- var
      main_vars <- unlist(stringr::str_split(interaction_var, "\\*"))

      x_betas <- betas %>%
        dplyr::filter(factor %in% c("(Intercept)", interaction_var, main_vars))

    } else {
      x_betas <- betas %>% dplyr::filter(factor %in% c("(Intercept)", var))
    }

    model_avg <- compute_model_avg(x, x_betas, current_baseline)

    one_table <- dplyr::bind_cols(
      factor = rep(var, length(mapping)),
      orig_level = names(mapping),
      actual_level = unname(mapping)) %>%
    dplyr::left_join(obs_avg, by = c("orig_level")) %>%
    dplyr::left_join(fitted_avg, by = c("orig_level")) %>%
    dplyr::left_join(model_avg, by = c("orig_level"))

    # nm <- paste0(i, " - ", var)
    factor_tables[[var]] <- one_table
  }

  factor_tables

}
