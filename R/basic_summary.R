basic_summary <- function(setup) {
  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target

  n_train <- nrow(train)
  avg_train <- round(mean(train[[target]]), 2)
  max_train <- round(max(train[[target]]), 2)
  min_train <- round(min(train[[target]]), 2)

  print("Train Data:")
  print(paste0("Number of Observations: ", n_train))
  print(paste0("Average Target Value: ", avg_train))
  print(paste0("Max. Target Value: ", max_train))
  print(paste0("Min. Target Value: ", min_train))
  print("")

  if(!is.null(test)) {
    n_test <- nrow(test)
    avg_test <- round(mean(test[[target]]), 2)
    max_test <- round(max(test[[target]]), 2)
    min_test <- round(min(test[[target]]), 2)

    print("Test Data:")
    print(paste0("Number of Observations: ", n_test))
    print(paste0("Average Target Value: ", avg_test))
    print(paste0("Max. Target Value: ", max_test))
    print(paste0("Min. Target Value: ", min_test))
  }
}
