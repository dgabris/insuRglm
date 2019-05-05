model_validate <- function(setup) {
  # TODO

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(!is.null(setup$data_test))

  models <- list()
  models$current_model <- setup$current_model

  ref_models_names <- names(setup$ref_models)
  lapply(ref_models_names, function(nm) {
    models[[nm]] <- setup$ref_models[[nm]]
  })


}
