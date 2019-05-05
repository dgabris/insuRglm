model_save <- function(setup, name = NULL) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  if(is.null(name)) {
    num_ref_models <- length(setup$ref_models)
    i <- num_ref_models + 1
    name <- paste0("model", i)
    message(paste0("Saving as 'model", i, "."))
  }

  setup$ref_models[[name]] <- setup$current_model

  setup
}
