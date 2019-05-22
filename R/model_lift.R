model_lift <- function(setup, method = c("train", "crossval", "test"), cv_stratified = FALSE,
                       cv_folds = 10, model = "current_model", buckets = 10) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(is.numeric(buckets) || is.integer(buckets))
  method <- match.arg(method)
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))
  if(method == "test") stopifnot(!is.null(setup$data_test))
  stopifnot(is.character(model))
  stopifnot(all(setdiff(model, "current_model") %in% names(setup$ref_models)))

  if(method == "test" && model != "current_model") {
    message("Showing only current model when using test set.")
    model <- "current_model"
  }

  if(inherits(setup, "offset_model")) {
    method <- "train"
    model <- "current_model"
  }

  model_list <- list()

  for(model_nm in setdiff(model, "current_model")) {
    model_list[[model_nm]] <- setup$ref_models[[model_nm]]
  }

  if("current_model" %in% model) model_list$current_model <- setup$current_model

  lift_buckets_list <- lift_buckets(setup, model_list, buckets, method, cv_folds, cv_stratified)

  lapply(seq_along(lift_buckets_list), function(i) {
    lift_plot(lift_buckets_list[[i]], names(lift_buckets_list)[[i]])
  })

}
