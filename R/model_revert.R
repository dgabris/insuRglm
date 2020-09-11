#' Revert back to a specific insuRglm model
#'
#' Reverts back to a specific saved insuRglm model within the setup (modeling) object.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param to Character scalar. Name of the saved model that we wish to revert to.
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @seealso \code{\link{model_save}}
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
#'   model_save('model1') %>%
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5,6))) %>%
#'   model_fit() %>%
#'   model_revert(to = 'model1') # from now on the two lines above have no effect (but they stay documented)
#'

model_revert <- function(setup, to) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")
  if(!(is.character(to) && length(to) == 1)) stop("'to' must be a character scalar")
  if(!to %in% names(setup$ref_models)) stop('Invalid model name provided')

  setup$current_model <- setup$ref_models[[to]]

  for(var in setup$simple_factors) {
    var_sym <- rlang::sym(var)
    wanted_attrs <- setup$current_model$data_attrs[[var]]
    wanted_class <- wanted_attrs$class[[1]]

    if(wanted_class == "simple_factor") {
      setup <- setup %>%
        factor_modify(!!var_sym := as_simple_factor(!!var_sym))
    }

    if(wanted_class == "custom_factor") {
      mapping <- wanted_attrs$mapping
      setup <- setup %>%
        factor_modify(!!var_sym := custom_factor(!!var_sym, mapping = mapping))
    }

    if(wanted_class == "variate") {
      type <- wanted_attrs$type
      mapping <- wanted_attrs$mapping
      degree <- wanted_attrs$degree
      prop_log <- wanted_attrs$prop_log

      setup <- setup %>%
        factor_modify(!!var_sym := variate(!!var_sym, type = type, prop_log = prop_log, mapping = mapping, degree = degree))
    }

    if(wanted_class == "offset") {
      mapping <- wanted_attrs$mapping
      setup <- setup %>%
        factor_modify(!!var_sym := offset_term(!!var_sym, mapping = mapping))
    }

  }

  setup
}
