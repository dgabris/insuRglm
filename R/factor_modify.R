#' Modify predictor in the current model formula
#'
#' Modifies one or more predictors in the current model formula.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param ... Expression. One or more named expressions (similar to dplyr::mutate).
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @seealso \code{\link{custom_factor}}, \code{\link{variate}}, \code{\link{as_simple_factor}},
#' \code{\link{offset}}
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
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) %>%
#'   model_fit()
#'

factor_modify <- function(setup, ...) {
  dots <- rlang::enexprs(...)

  setup$data_train <- setup$data_train %>%
    dplyr::mutate(!!!dots)

  if(!is.null(setup$data_test)) {
    setup$data_test <- setup$data_test %>%
      dplyr::mutate(!!!dots)
  }

  setup
}
