remove <- function(setup, var_symbol) {
    stopifnot(inherits(setup, "setup"))

    remove_var <- as.character(enexpr(var_symbol))
    stopifnot(remove_var %in% setup$predictors)

    setup$predictors <- setdiff(setup$predictors, remove_var)

    setup
}
