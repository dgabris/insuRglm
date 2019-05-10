model_lift <- function(setup, train_test = c("train", "test"), model = c("current", "all"), buckets = 10) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(is.numeric(buckets) || is.integer(buckets))
  train_test <- match.arg(train_test)
  if(train_test == "test") stopifnot(!is.null(setup$data_test))
  model <- match.arg(model)

  if(train_test == "train") {
    actual <- setup$data_train[[setup$target]]
    weight <- setup$data_train[[setup$weight]]
  } else {
    actual <- setup$data_test[[setup$target]]
    weight <- setup$data_test[[setup$weight]]
  }

  model_list <- list()

  if(model == "all") {
    for(ref_model_nm in names(setup$ref_models)) {
      model_list[[ref_model_nm]] <- setup$ref_models[[ref_model_nm]]
    }
  }

  model_list$current_model <- setup$current_model

  lapply(names(model_list), function(x) {
    model_nm <- x
    model <- model_list[[model_nm]]

    if(train_test == "train") {
      expected <- model$train_predictions
    } else {
      expected <- model$test_predictions
    }

    lift_plot(actual, expected, weight, buckets, paste0(model_nm, " (", train_test, ")"))

  })


}
