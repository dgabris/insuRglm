#' Convert modified predictor back into simple predictor
#'
#' Converts custom factor, variate or offset back into the simple predictor.
#' This may be also needed as a transition step when converting for example from custom factor to variate
#'
#' @param x Unquoted symbol. Predictor to be converted back to simple factor. Must be present in the modeling dataset.
#'
#' @return Original vector with updated attributes.
#' @export
#'
#' @seealso \code{\link{factor_modify}}
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
#'   factor_add(veh_value) %>%
#'   model_fit() %>%
#'   model_save('model1') %>%
#'   factor_modify(veh_value = custom_factor(veh_value, mapping = c(1, 2, 3, 3, 3))) %>%
#'   model_fit() %>%
#'   model_save('model2') %>%
#'   factor_modify(veh_value = as_simple_factor(veh_value)) %>%
#'   factor_modify(veh_value = variate(veh_value, type = 'prop')) %>%
#'   model_fit()
#'

as_simple_factor <- function(x) {
  stopifnot(inherits(x, "simple_factor"))

  attr(x, "mapping") <- NULL
  class(x) <- setdiff(class(x), c("variate", "custom_factor", "offset"))

  x
}
