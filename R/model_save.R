#' Save fitted insuRglm model for later use
#'
#' Saves already fitted insuRglm model for later use, which might be comparison using \code{model_compare} or
#' returning back to a previous model using \code{model_revert}.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param name Character scalar. Model will be saved under this name.
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @seealso \code{\link{model_compare}}, \code{\link{model_revert}}
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
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) %>%
#'   model_fit()
#'
#' modeling %>%
#'   model_compare(with = 'model1')
#'

model_save <- function(setup, name = NULL) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")

  if(is.null(name)) {
    num_ref_models <- length(setup$ref_models)
    i <- num_ref_models + 1
    name <- paste0("model", i)
    message(paste0("Saving as 'model", i, "."))
  } else {
    if(!(is.character(name) && length(name) == 1)) stop("'name' must be a character scalar")
  }

  setup$ref_models[[name]] <- setup$current_model

  setup
}
