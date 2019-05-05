compute_obs_avg <- function(x, target_vector, weight_vector) {
  # TODO determine logic for picking base level in presence of custom factor / variate
  # TODO determine whether it makes sense to compute rescaled value for observed average

  orig_levels <- attr(x, "orig_levels")
  base_model_level <- orig_levels[[1]]

  obs_avg_df <- dplyr::bind_cols(x = x, target = target_vector, weight = weight_vector) %>%
    dplyr::group_by(x) %>%
    dplyr::summarize(
      weight_sum = sum(weight),
      obs_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
      obs_avg_lin_nonrescaled = log(obs_avg_pred_nonrescaled)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(weight = weight_sum)

  ind <- orig_levels == base_model_level
  base_model_value_pred <- obs_avg_df$obs_avg_pred_nonrescaled[ind]
  base_model_value_lin <- obs_avg_df$obs_avg_lin_nonrescaled[ind]

  obs_avg_df %>%
    dplyr::mutate(
      obs_avg_pred_rescaled = obs_avg_pred_nonrescaled / base_model_value_pred,
      obs_avg_lin_rescaled = obs_avg_lin_nonrescaled - base_model_value_lin
    ) %>%
    dplyr::rename(orig_level = x) %>%
    dplyr::mutate(orig_level = as.character(orig_level))
}
