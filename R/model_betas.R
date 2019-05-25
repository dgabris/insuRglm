model_betas <- function(setup, triangles = FALSE) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  if(triangles) {
    setup$current_model$beta_triangles
  } else {
    setup$current_model$betas
  }

}
