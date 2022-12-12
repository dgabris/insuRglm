factor_tables <- function(setup, betas, current_baseline, predictions, data_attrs) {
  train <- setup$data_train
  target <- setup$target
  weight <- setup$weight

  target_vector <- train[[target]]
  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  simple_factors <- setup$simple_factors
  predictors <- setup$current_model$predictors

  # vars <- c(.predictors, setdiff(simple_factors, predictors))
  vars <- c(predictors, setdiff(simple_factors, predictors))

  obs_avg_tables <- setup$obs_avg_tables[vars]

  vars <- as.list(train[vars])

  factor_tables <- furrr::future_map2(vars, obs_avg_tables, carrier::crate(function(x, obs_avg) {
    var <- attr(x, "var_nm")

    if(inherits(x, "custom_factor") || inherits(x, "interaction") ||
       inherits(x, "offset")) {
      mapping <- attr(x, "mapping")

    } else if(inherits(x, "variate")) {
      mapping_df <- attr(x, "mapping")
      mapping <- stats::setNames(mapping_df$actual_level, mapping_df$orig_level)

    } else {
      orig_levels <- attr(x, "orig_levels")
      mapping <- stats::setNames(orig_levels, orig_levels)
    }

    fitted_avg <- compute_fitted_avg(x, predictions, weight_vector)

    if(inherits(x, "interaction")) {
      main_effects <- attr(x, "main_effects")
      components <- attr(x, "components")

      x_betas <- betas %>%
        dplyr::filter(factor %in% c("(Intercept)", main_effects, components, var))

      join_keys <- main_effects

      model_avg <- compute_model_avg(x, x_betas, current_baseline, data_attrs = data_attrs)

      obs_avg %>%
        dplyr::left_join(fitted_avg, by = join_keys) %>%
        dplyr::left_join(model_avg, by = join_keys)

    } else {
      x_betas <- betas %>% dplyr::filter(factor %in% c("(Intercept)", var))
      join_keys <- "orig_level"

      model_avg <- compute_model_avg(x, x_betas, current_baseline)

      dplyr::bind_cols(
        factor = rep(var, length(mapping)),
        orig_level = names(mapping),
        actual_level = unname(mapping)) %>%
        dplyr::left_join(obs_avg, by = join_keys) %>%
        dplyr::left_join(fitted_avg, by = join_keys) %>%
        dplyr::left_join(model_avg, by = join_keys)
    }

  },
  current_baseline = current_baseline,
  betas = betas,
  target_vector = target_vector,
  weight_vector = weight_vector,
  predictions = predictions,
  data_attrs = data_attrs,
  compute_fitted_avg = compute_fitted_avg,
  compute_model_avg = compute_model_avg,
  `%>%` = `%>%`
  ),
  .options = furrr::furrr_options(globals = FALSE)
  )

  factor_tables

}
