cramers_v <- function(x, y) {
  x_catg_count <- length(unique(x))
  y_catg_count <- length(unique(y))

  chisq_stat <- chisq.test(x, y, correct = FALSE)$statistic
  denominator <- length(x) * (min(x_catg_count, y_catg_count) - 1)

  chisq_stat / denominator
}
