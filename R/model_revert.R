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
#'   data_train = train,
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
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(to %in% names(setup$ref_models))

  setup$current_model <- setup$ref_models[[to]]

  setup
}
