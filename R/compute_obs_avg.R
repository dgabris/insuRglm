compute_obs_avg <- function(x, target_vector, weight_vector, by = NULL) {

  orig_levels <- attr(x, "orig_levels")

  if(!is.null(by)) {
    obs_avg_df <- dplyr::bind_cols(x = x, by = by, target = target_vector, weight = weight_vector) %>%
      dplyr::group_by(x, by) %>%
      dplyr::summarize(
        weight_sum = sum(weight),
        obs_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
        obs_avg_lin_nonrescaled = log(obs_avg_pred_nonrescaled)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_lvl_idx = (by == attr(.env$by, "base_level"))) %>%
      dplyr::group_by(x) %>%
      dplyr::mutate(
        obs_avg_pred_rescaled = obs_avg_pred_nonrescaled / obs_avg_pred_nonrescaled[base_lvl_idx],
        obs_avg_lin_rescaled = obs_avg_lin_nonrescaled - obs_avg_lin_nonrescaled[base_lvl_idx]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(by = as.character(by))
  } else {
    obs_avg_df <- dplyr::bind_cols(x = x, target = target_vector, weight = weight_vector) %>%
      dplyr::group_by(x) %>%
      dplyr::summarize(
        weight_sum = sum(weight),
        obs_avg_pred_nonrescaled = sum(target * weight) / sum(weight),
        obs_avg_lin_nonrescaled = log(obs_avg_pred_nonrescaled)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_lvl_idx = (x == attr(.env$x, "base_level"))) %>%
      dplyr::mutate(
        obs_avg_pred_rescaled = obs_avg_pred_nonrescaled / obs_avg_pred_nonrescaled[base_lvl_idx],
        obs_avg_lin_rescaled = obs_avg_lin_nonrescaled - obs_avg_lin_nonrescaled[base_lvl_idx]
      )
  }

  obs_avg_df <- obs_avg_df %>%
    dplyr::rename(weight = weight_sum) %>%
    dplyr::select(-base_lvl_idx) %>%
    dplyr::rename(orig_level = x) %>%
    dplyr::mutate(orig_level = as.character(orig_level))

  if(inherits(x, "interaction")) {
    base_levels <- stringr::str_split(attr(x, "base_level"), "__", simplify = TRUE) %>%
      as.vector() %>%
      paste0("(__)?(,", ., ",)(__)?")

    base_lvl_regex <- paste0(base_levels, collapse = "|")

    obs_avg_df <- obs_avg_df %>%
      dplyr::mutate(
        obs_avg_pred_rescaled = dplyr::if_else(stringr::str_detect(orig_level, base_lvl_regex), 1, obs_avg_pred_rescaled),
        obs_avg_lin_rescaled = dplyr::if_else(stringr::str_detect(orig_level, base_lvl_regex), 0, obs_avg_lin_rescaled)
      ) %>%
      tidyr::separate(orig_level, into = attr(x, "main_effects"), sep = "__") %>%
      dplyr::mutate_at(attr(x, "main_effects"), function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2"))
  }

  obs_avg_df

}
