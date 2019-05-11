model_lift <- function(setup, method = c("train", "crossval", "test"),
                       cv_folds = 10, model = c("current", "all"), buckets = 10) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(is.numeric(buckets) || is.integer(buckets))
  method <- match.arg(method)
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))
  if(method == "test") stopifnot(!is.null(setup$data_test))
  model <- match.arg(model)

  if(method == "test" && model == "all") {
    message("Showing only current model when using test set.")
    model <- "current"
  }

  if(method %in% c("train", "crossval")) {
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

  if(method == "crossval") {
    n_model_fits <- length(model_list) * cv_folds

    message(
      paste0(
        "This may take a while. ", n_model_fits,
        " models need to be fitted to produce these liftcharts."
      ))
  }

  lapply(names(model_list), function(x) {
    model_nm <- x
    model <- model_list[[model_nm]]

    if(method == "train") {
      expected <- model$train_predictions

    } else if(method == "crossval") {
      expected <- crossval_predict(setup$data_train, model, cv_folds)

    } else {
      expected <- model$test_predictions
    }

    lift_plot(actual, expected, weight, buckets, paste0(model_nm, " (", method, ")"))

  })


}
