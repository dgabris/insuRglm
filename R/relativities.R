relativities <- function(factor_tables) {
  Filter(factor_tables, f = function(x) {
    all(!is.na(x$model_avg_pred_rescaled))
  }) %>%
  lapply(function(x) {
    x %>%
      dplyr::select(orig_level, relativity = model_avg_pred_rescaled)
  })
}
