model_save <- function(setup, model_name = NULL) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  if(is.null(name)) {
    num_ref_models <- length(setup$ref_models)
    i <- num_ref_models + 1
    model_name <- paste0("model", i)
  }

  setup$ref_models[[model_name]] <- setup$current_model

  setup
}
