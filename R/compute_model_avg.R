compute_model_avg <- function(x, x_betas) {

  # when a variable is NOT a predictor, x_betas will be intercept only
  # in that case return df with NAs
  if(nrow(x_betas) == 1) {
    orig_levels <- attr(x, "orig_levels")

    model_avg_df <- dplyr::bind_cols(
      orig_level = orig_levels,
      model_avg_pred_nonrescaled = rep(NA_real_, length(orig_levels)),
      model_avg_lin_nonrescaled = rep(NA_real_, length(orig_levels)),
      model_avg_pred_rescaled = rep(NA_real_, length(orig_levels)),
      model_avg_lin_rescaled = rep(NA_real_, length(orig_levels))
    )

    return(model_avg_df)
  }

  intercept_row <- x_betas %>% dplyr::filter(factor == "(Intercept)")
  intercept_estimate <- intercept_row$estimate[[1]]

  main_rows <- x_betas %>% dplyr::filter(factor != "(Intercept)") %>%
    dplyr::select(actual_level, estimate)

  if(inherits(x, "custom_factor")) {
    mapping <- attr(x, "mapping")

    model_avg_df <- dplyr::bind_cols(
      orig_level = names(mapping),
      actual_level = as.character(unname(mapping))
      ) %>%
      dplyr::left_join(main_rows, by = c("actual_level")) %>%
      dplyr::mutate(estimate = dplyr::coalesce(estimate, 0)) %>%
      dplyr::mutate(
        model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate),
        model_avg_lin_nonrescaled = intercept_estimate + estimate,
        model_avg_pred_rescaled = exp(estimate),
        model_avg_lin_rescaled = estimate
      ) %>%
      dplyr::select(-actual_level, -estimate)

  } else if(inherits(x, "variate")) {
    stopifnot(nrow(main_rows) == 1)

    estimate <- main_rows$estimate[[1]]
    mapping <- attr(x, "mapping")

    model_avg_df <- dplyr::bind_cols(
      orig_level = names(mapping),
      actual_level = unname(mapping),
      estimate = rep(estimate, length(mapping))
    ) %>%
    #dplyr::mutate_at(c("actual_level", "estimate"), as.numeric) %>%
    dplyr::mutate(
      model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate * actual_level),
      model_avg_lin_nonrescaled = intercept_estimate + (estimate * actual_level),
      model_avg_pred_rescaled = exp(estimate * actual_level),
      model_avg_lin_rescaled = estimate * actual_level
    ) %>%
    dplyr::select(-actual_level, -estimate)

  } else if(inherits(x, "simple_factor")) {
    model_avg_df <- tibble::tibble(
      orig_level = attr(x, "orig_levels")
    ) %>%
    dplyr::left_join(main_rows, by = c("orig_level" = "actual_level")) %>%
    dplyr::mutate(estimate = dplyr::coalesce(estimate, 0)) %>%
    dplyr::mutate(
      model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate),
      model_avg_lin_nonrescaled = intercept_estimate + estimate,
      model_avg_pred_rescaled = exp(estimate),
      model_avg_lin_rescaled = estimate
    ) %>%
    dplyr::select(-estimate)
  }

  model_avg_df
}