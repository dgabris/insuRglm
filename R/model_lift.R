model_lift <- function(setup, buckets = 10) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(is.numeric(buckets) || is.integer(buckets))

  weight <- setup$data_train[[setup$weight]]
  actual <- setup$data_train[[setup$target]]
  expected <- setup$current_model$train_predictions

  deciles <- dplyr::bind_cols(
    weight = weight,
    actual = actual,
    expected = expected
  ) %>%
  dplyr::mutate(
    bucket = findInterval(
      expected,
      quantile(expected, probs = seq(0, 1, by = 1 / buckets)),
      all.inside = TRUE
    )
  ) %>%
  dplyr::group_by(bucket) %>%
  dplyr::summarize(
    actual = sum(actual * weight) / sum(weight),
    expected = sum(expected * weight) / sum(weight)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::gather(key = type, value = target, actual, expected)

  deciles %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(bucket), y = target)) +
    ggplot2::geom_bar(ggplot2::aes(fill = type), stat = "identity", position = "dodge") +
    ggplot2::labs(x = "Bucket", y = "Target", fill = "Type")
}
