target_dist <- function(setup, ...) {
  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target

  result <- list()

  result$train <- ggplot(data = train) +
    geom_histogram(aes(target), ...)

  if(!is.null(test)) {
    result$test <- ggplot(data = test) +
      geom_histogram(aes(target), ...)
  }

  result
}
