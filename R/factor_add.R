factor_add <- function(setup, var_symbol) {
  stopifnot(inherits(setup, "setup"))

  new_var <- as.character(rlang::ensym(var_symbol))
  stopifnot(new_var %in% setup$simple_factors)

  predictors <- setup$current_model$predictors

  if(new_var %in% predictors) message(paste0("Can't add '", new_var, "'. It's already among predictors."))
  setup$current_model$predictors <- unique(c(predictors, new_var))

  setup
}
