selection_metric <- function(df, metric = c("rmse")) {
  stopifnot(inherits(df, "data.frame"))
  stopifnot(all(c("actual", "expected", "weight") %in% colnames(df)))
  metric <- match.arg(metric)

  if(metric == "rmse") {
    df %>%
      dplyr::mutate(squared_residual = (actual - expected)^2) %>%
      dplyr::summarize(rmse = sqrt(sum(squared_residual) / n())) %>%
      unlist(use.names = FALSE)
  }

}
