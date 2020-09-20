compute_fitted_avg <- function(x, predictions, weight_vector, by = NULL) {

  orig_levels <- attr(x, "orig_levels")

  if(!is.null(by)) {
    fitted_avg_df <- dplyr::bind_cols(x = x, by = by, target = predictions, weight = weight_vector) %>%
      dplyr::group_by(x, by) %>%
      dplyr::summarize(
        fitted_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
        fitted_avg_lin_nonrescaled = log(fitted_avg_pred_nonrescaled)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_lvl_idx = (by == attr(.env$by, "base_level"))) %>%
      dplyr::group_by(x) %>%
      dplyr::mutate(
        fitted_avg_pred_rescaled = fitted_avg_pred_nonrescaled / fitted_avg_pred_nonrescaled[base_lvl_idx],
        fitted_avg_lin_rescaled = fitted_avg_lin_nonrescaled - fitted_avg_lin_nonrescaled[base_lvl_idx]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(by = as.character(by))
  } else {
    fitted_avg_df <- dplyr::bind_cols(x = x, target = predictions, weight = weight_vector) %>%
      dplyr::group_by(x) %>%
      dplyr::summarize(
        fitted_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
        fitted_avg_lin_nonrescaled = log(fitted_avg_pred_nonrescaled)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_lvl_idx = (x == attr(.env$x, "base_level"))) %>%
      dplyr::mutate(
        fitted_avg_pred_rescaled = fitted_avg_pred_nonrescaled / fitted_avg_pred_nonrescaled[base_lvl_idx],
        fitted_avg_lin_rescaled = fitted_avg_lin_nonrescaled - fitted_avg_lin_nonrescaled[base_lvl_idx]
      )
  }

  fitted_avg_df %>%
    dplyr::select(-base_lvl_idx) %>%
    dplyr::rename(orig_level = x) %>%
    dplyr::mutate(orig_level = as.character(orig_level))
}
