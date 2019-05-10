lift_plot <- function(actual, expected, weight, buckets, title) {

  stopifnot(is.numeric(actual) || is.integer(actual))
  stopifnot(is.numeric(expected) || is.integer(expected))
  stopifnot(is.numeric(weight) || is.integer(weight))

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
    ggplot2::labs(x = "Bucket", y = "Target", fill = "Type") +
    ggplot2::ggtitle(paste0("Lift Chart - ", title)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45)
    )

}
