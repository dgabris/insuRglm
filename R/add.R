add <- function(setup, var_symbol) {
  stopifnot(inherits(setup, "setup"))

  new_var <- as.character(enexpr(var_symbol))

  setup$predictors <- c(setup$predictors, new_var)

  setup
}
