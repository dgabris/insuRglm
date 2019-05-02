basic_summary <- function(setup) {
  train <- setup$data_train
  test <- setup$data_test
  target <- setup$target

  print("Train Data:")
  print(paste0("Number of Observations: ", nrow(train)))
  print(paste0("Average Target Value: ", mean(train[[target]])))
  print(paste0("Max. Target Value: ", max(train[[target]])))
  print(paste0("Min. Target Value: ", min(train[[target]])))
  print("")

  if(!is.null(test)) {
    print("Test Data:")
    print(paste0("Number of Observations: ", nrow(test)))
    print(paste0("Average Target Value: ", mean(test[[target]])))
    print(paste0("Max. Target Value: ", max(test[[target]])))
    print(paste0("Min. Target Value: ", min(test[[target]])))
  }
}
