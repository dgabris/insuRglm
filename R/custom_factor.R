#' Simplify to custom factor
#'
#' Simplifies simple factor to custom factor, which usually means that some of the original levels are merged together.
#' Must be used within \code{factor_modify} function. This is usually done with categorical variables where order of levels doesn't matter.
#'
#' @param x Unquoted symbol. Predictor to be simplified. Must be present in the modeling dataset.
#' @param mapping Integer vector. Mapping to use for simplification. Length must be equal to length of unique levels of the predictor.
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
#'   data_train = sev_train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(area) %>%
#'   model_fit() %>%
#'   model_save('model1') %>%
#'   factor_modify(area = custom_factor(area, mapping = c(1, 2, 3, 1, 4, 4))) %>%
#'   model_fit()
#'

custom_factor <- function(x, mapping) {
  if(!inherits(x, 'simple_factor')) stop('Please use the predictor from the dataset')

  if(inherits(x, 'custom_factor') || inherits(x, 'variate') || inherits(x, 'offset')) {
    x <- as_simple_factor(x)
  }

  if(!is.numeric(mapping)) stop("'mapping' must be a numeric vector")
  if(!length(attr(x, 'orig_levels')) == length(mapping)) stop("'mapping' must be of same length as number of levels")

  if(is.null(names(mapping))) {
    names(mapping) <- attr(x, "orig_levels")
  }

  attr(x, "mapping") <- mapping
  class(x) <- if(!inherits(x, "custom_factor")) c("custom_factor", class(x)) else class(x)

  x
}
