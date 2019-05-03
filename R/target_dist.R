target_dist <- function(setup, ...) {
  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target
  target_sym <- rlang::sym(target)

  # TODO overlay train and test on the same chart

  result <- list()

  result$train <- ggplot2::ggplot(data = train) +
    ggplot2::geom_histogram(ggplot2::aes(!!target_sym), ...)

  if(!is.null(test)) {
    result$test <- ggplot2::ggplot(data = test) +
      ggplot2::geom_histogram(ggplot2::aes(!!target_sym), ...)
  }

  result
}
