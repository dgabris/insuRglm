#' Inspect insuRglm model coefficients
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param triangles Boolean scalar. Show beta coefficient differences instead. These are expressed using standard error percentage.
#'
#' @return Either dataframe containing model coefficients or matrix-like dataframe containing beta coefficient differences
#' @export
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
#'   model_fit()
#'
#' modeling %>%
#'   model_betas()
#'
#' modeling %>%
#'   model_betas(triangles = TRUE)
#'

model_betas <- function(setup, triangles = FALSE) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  if(triangles) {
    setup$current_model$beta_triangles
  } else {
    setup$current_model$betas
  }

}
