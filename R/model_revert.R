model_revert <- function(setup, to) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(to %in% names(setup$ref_models))

  setup$current_model <- setup$ref_models[[to]]

  setup
}
