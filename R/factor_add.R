factor_add <- function(setup, var_symbol) {
  stopifnot(inherits(setup, "setup"))

  new_var <- as.character(rlang::ensym(var_symbol))
  stopifnot(new_var %in% setup$simple_factors)

  if(new_var %in% setup$predictors) message(paste0("Can't add '", new_var, "'. It's already among predictors."))
  setup$predictors <- unique(c(setup$predictors, new_var))

  setup
}
