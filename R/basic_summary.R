basic_summary <- function(setup) {

  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target
  weight <- setup$weight

  n_train <- nrow(train)
  w_avg_train <- round(sum(train[[target]] * train[[weight]]) / sum(train[[weight]]), 2)
  max_train <- round(max(train[[target]]), 2)
  min_train <- round(min(train[[target]]), 2)

  print("Train Data:")
  print(paste0("Number of Observations: ", n_train))
  print(paste0("Weighted Average Target: ", w_avg_train))
  print(paste0("Max. Target: ", max_train))
  print(paste0("Min. Target: ", min_train))
  print("")

  if(!is.null(test)) {
    n_test <- nrow(test)
    w_avg_test <- round(sum(test[[target]] * test[[weight]]) / sum(test[[weight]]), 2)
    max_test <- round(max(test[[target]]), 2)
    min_test <- round(min(test[[target]]), 2)

    print("Test Data:")
    print(paste0("Number of Observations: ", n_test))
    print(paste0("Weighted Average Target: ", w_avg_test))
    print(paste0("Max. Target: ", max_test))
    print(paste0("Min. Target: ", min_test))
  }
}
