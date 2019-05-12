compute_fitted_avg <- function(x, predictions, weight_vector) {

  orig_levels <- attr(x, "orig_levels")
  base_model_level <- attr(x, "base_level")

  fitted_avg_df <- dplyr::bind_cols(x = x, target = predictions, weight = weight_vector) %>%
    dplyr::group_by(x) %>%
    dplyr::summarize(
      fitted_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
      fitted_avg_lin_nonrescaled = log(fitted_avg_pred_nonrescaled)
    ) %>%
    dplyr::ungroup()

  ind <- orig_levels == base_model_level
  base_model_value_pred <- fitted_avg_df$fitted_avg_pred_nonrescaled[ind]
  base_model_value_lin <- fitted_avg_df$fitted_avg_lin_nonrescaled[ind]

  fitted_avg_df %>%
    dplyr::mutate(
      fitted_avg_pred_rescaled = fitted_avg_pred_nonrescaled / base_model_value_pred,
      fitted_avg_lin_rescaled = fitted_avg_lin_nonrescaled - base_model_value_lin
    ) %>%
    dplyr::rename(orig_level = x) %>%
    dplyr::mutate(orig_level = as.character(orig_level))
}
