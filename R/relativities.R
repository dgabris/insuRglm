relativities <- function(factor_tables) {
  # TODO - add intercept relativity as the first member in the list

  Filter(factor_tables, f = function(x) {
    all(!is.na(x$model_avg_pred_rescaled))
  }) %>%
  lapply(function(x) {
    x %>%
      dplyr::select(factor, orig_level, relativity = model_avg_pred_rescaled)
  })
}
