model_betas <- function(setup) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  setup$current_model$betas
}
