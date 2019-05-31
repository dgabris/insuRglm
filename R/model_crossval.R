model_crossval <- function(setup, cv_folds = 10, stratified = FALSE) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  train <- setup$data_train

  for(model_nm in names(setup$ref_models)) {
    model <- setup$ref_models[[model_nm]]
    setup$ref_models[[model_nm]]$cv_predictions <- crossval_predict(train, model, cv_folds, stratified)
  }

  current_model <- setup$current_model
  setup$current_model$cv_predictions <- crossval_predict(train, current_model, cv_folds, stratified)

  setup
}
