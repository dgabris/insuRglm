model_lift <- function(setup, data = c("train", "crossval"), model = c("current", "all"), buckets = 10, weighted = TRUE) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  data <- match.arg(data)
  model <- match.arg(model)
  stopifnot(is.numeric(buckets) || is.integer(buckets))

  if(inherits(setup, "offset_model")) {
    data <- "train"
    model <- "current"
    message("Offset model has train (formerly test) data only.")
  }

  if(data == "crossval" && is.null(setup$current_model$cv_predictions)) {
    message("No CV predictions found. Please run 'model_crossval()' first.")
    return(setup)
  }

  train <- setup$data_train
  actual <- train[[setup$target]]
  weight_vector <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]

  model_list <- list()

  if(model == "all") {
    for(model_nm in names(setup$ref_models)) {
      model_list[[model_nm]] <- setup$ref_models[[model_nm]]
    }
  }

  model_list$current_model <- setup$current_model

  lapply(names(model_list), function(model_nm) {
    if(data == "train") {
      expected <- model_list[[model_nm]]$train_predictions
    } else {
      expected <- model_list[[model_nm]]$cv_predictions
    }

    lift_buckets(actual, expected, weight_vector, buckets, weighted) %>%
      tidyr::gather(key = type, value = target, actual, expected) %>%
      lift_plot(title = paste0(model_nm, " (", data, ")"))
  })

}
