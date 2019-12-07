#' Remove predictor from the current model formula
#'
#' Removes the predictor from the list of predictors. These will be used in model formula when \code{model_fit} is called.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param var_symbol Unquoted symbol. Predictor to be removed. Must be present in the modeling dataset.
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = sev_train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat) %>%
#'   model_fit() %>%
#'   factor_remove(agecat)
#'

factor_remove <- function(setup, var_symbol) {
    if(!inherits(setup, 'setup')) stop('Setup object is not correct')

    remove_var <- as.character(rlang::ensym(var_symbol))
    predictors <- setup$current_model$predictors

    if(!remove_var %in% predictors) message(paste0("Can't remove '", remove_var, "'. It's not among predictors."))

    setup$current_model$predictors <- setdiff(predictors, remove_var)

    setup
}
