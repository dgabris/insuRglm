relativities <- function(factor_tables, current_baseline) {

  base_value <- exp(current_baseline)

  base_value_list <- list(`base_value` = base_value)

  relativity_list <- Filter(factor_tables, f = function(x) {
    all(!is.na(x$model_avg_pred_rescaled))
  }) %>%
  lapply(function(x) {

    if(colnames(x)[[1]] == "factor") {
      # most cases
      factor_nm <- x$factor[[1]]
      factor_nm_sym <- rlang::sym(factor_nm)

      x %>%
        dplyr::select(!!factor_nm_sym := orig_level, relativity = model_avg_pred_rescaled, weight)

    } else {
      # interaction
      x %>%
        dplyr::select(
          -starts_with("obs_avg"),
          -starts_with("fitted_avg"),
          -contains("model_avg_lin"),
          -model_avg_pred_nonrescaled
        ) %>%
        dplyr::rename(relativity = model_avg_pred_rescaled) %>%
        dplyr::select(dplyr::all_of(setdiff(colnames(.), "weight")), weight)
    }

  })

  c(base_value_list, relativity_list)
}
