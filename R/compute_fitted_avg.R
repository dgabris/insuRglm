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

  fitted_avg_df <- fitted_avg_df %>%
    dplyr::select(-base_lvl_idx) %>%
    dplyr::rename(orig_level = x) %>%
    dplyr::mutate(orig_level = as.character(orig_level))

  if(inherits(x, "interaction")) {
    base_levels <- stringr::str_split(attr(x, "base_level"), "__", simplify = TRUE) %>%
      as.vector() %>%
      paste0("(__)?(,", ., ",)(__)?")

    base_lvl_regex <- paste0(base_levels, collapse = "|")

    fitted_avg_df <- fitted_avg_df %>%
      dplyr::mutate(
        fitted_avg_pred_rescaled = dplyr::if_else(stringr::str_detect(orig_level, base_lvl_regex), 1, fitted_avg_pred_rescaled),
        fitted_avg_lin_rescaled = dplyr::if_else(stringr::str_detect(orig_level, base_lvl_regex), 0, fitted_avg_lin_rescaled)
      ) %>%
      tidyr::separate(orig_level, into = attr(x, "main_effects"), sep = "__") %>%
      dplyr::mutate_at(attr(x, "main_effects"), function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2"))
  }

  fitted_avg_df
}
