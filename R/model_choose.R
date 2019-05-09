model_choose <- function(setup, name = NULL) {
  stopifnot(inherits(setup, "setup"))
  if(!is.null(name)) stopifnot(name %in% names(setup$ref_models))

  final_model <- if(is.null(name)) setup$current_model else setup$ref_models[[name]]
  setup$current_model <- final_model

  # erase reference models
  setup$ref_models <- NULL

  # combine train and test for the final model fit
  setup$data_train <- dplyr::bind_rows(setup$data_train, setup$data_test)
  setup$data_test <- NULL

  class(setup) <- c("final_model", class(setup))

  setup
}
