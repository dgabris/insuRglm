basic_summary <- function(setup) {

  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target

  n_train <- nrow(train)
  train_weight <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]

  w_avg_train <- round(sum(train[[target]] * train_weight) / sum(train_weight), 2)
  max_train <- round(max(train[[target]]), 2)
  min_train <- round(min(train[[target]]), 2)

  cat("Train Data:")
  cat("\n")
  cat(paste0("Number of Observations: ", n_train))
  cat("\n")
  cat(paste0("Weighted Average Target: ", w_avg_train))
  cat("\n")
  cat(paste0("Max. Target: ", max_train))
  cat("\n")
  cat(paste0("Min. Target: ", min_train))
  cat("\n")

  if(!is.null(test)) {
    n_test <- nrow(test)
    test_weight <- if(is.null(setup$weight)) rep(1, nrow(test)) else test[[setup$weight]]

    w_avg_test <- round(sum(test[[target]] * test_weight) / sum(test_weight), 2)
    max_test <- round(max(test[[target]]), 2)
    min_test <- round(min(test[[target]]), 2)

    cat("Test Data:")
    cat("\n")
    cat(paste0("Number of Observations: ", n_test))
    cat("\n")
    cat(paste0("Weighted Average Target: ", w_avg_test))
    cat("\n")
    cat(paste0("Max. Target: ", max_test))
    cat("\n")
    cat(paste0("Min. Target: ", min_test))
    cat("\n")
  }
}
