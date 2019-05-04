factor_remove <- function(setup, var_symbol) {
    stopifnot(inherits(setup, "setup"))

    remove_var <- as.character(rlang::enexpr(var_symbol))
    if(!remove_var %in% setup$predictors) message(paste0("Can't remove '", remove_var, "'. It's not among predictors."))

    setup$predictors <- setdiff(setup$predictors, remove_var)

    setup
}
