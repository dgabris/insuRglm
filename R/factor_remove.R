factor_remove <- function(setup, var_symbol) {
    stopifnot(inherits(setup, "setup"))

    remove_var <- as.character(rlang::ensym(var_symbol))
    predictors <- setup$current_model$predictors

    if(!remove_var %in% predictors) message(paste0("Can't remove '", remove_var, "'. It's not among predictors."))

    setup$current_model$predictors <- setdiff(predictors, remove_var)

    setup
}
