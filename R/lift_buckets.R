lift_buckets <- function(actual, expected, weight, buckets, weighted) {
  stopifnot(is.numeric(actual) || is.integer(actual))
  stopifnot(is.numeric(expected) || is.integer(expected))
  stopifnot(is.numeric(weight) || is.integer(weight))
  stopifnot(length(actual) == length(expected))
  stopifnot(length(actual) == length(weight))
  stopifnot(is.numeric(buckets) || is.integer(buckets))

  df_prep <- dplyr::bind_cols(
    weight = weight,
    actual = actual,
    expected = expected
  ) %>%
  dplyr::arrange(expected)

  if(weighted) {
    df_buckets <- df_prep %>%
      dplyr::mutate(
        bucket = ceiling((cumsum(weight) / sum(weight)) * buckets)
      )
  } else {
    df_buckets <- df_prep %>%
      dplyr::mutate(
        bucket = findInterval(
          expected,
          quantile(expected, probs = seq(0, 1, by = 1 / buckets)),
          all.inside = TRUE
      ))
  }

  df_buckets %>%
    dplyr::group_by(bucket) %>%
    dplyr::summarize(
      actual = sum(actual * weight) / sum(weight),
      expected = sum(expected * weight) / sum(weight),
      weight = sum(weight)
    ) %>%
    dplyr::ungroup()

}
