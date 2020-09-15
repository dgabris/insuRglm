#' Simplify to variate
#'
#' Simplifies simple factor to variate, which usually means that monotonic trend is enforced and some of the levels may be merged together.
#' Must be used within \code{factor_modify} function. This is usually done with originally continuous variables.
#'
#' @param x Unquoted symbol. Predictor to be simplified. Must be present in the modeling dataset.
#' @param type Character scalar. One of \code{prop} or \code{non_prop}, specifying proportional and unproportional variate.
#' @param prop_log Boolean scalar. Whether the proportional variate should be logged (recommended).
#' @param mapping Integer vector. Mapping to use for simplification. Length must be equal to length of unique levels of the predictor.
#' @param degree Integer scalar. Polynomial degree to use.
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
#'   factor_add(agecat) %>%
#'   model_fit() %>%
#'   model_save('model1') %>%
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) %>%
#'   model_fit()
#'

variate <- function(x, type = c("prop", "non_prop"), prop_log = TRUE,  mapping = NULL, degree = 1) {
  if(!inherits(x, 'simple_factor')) stop('Please use the predictor from the dataset')

  if(inherits(x, 'custom_factor') || inherits(x, 'variate') || inherits(x, 'offset')) {
    x <- as_simple_factor(x)
  }

  if(!is.null(mapping)) {
    if(!is.numeric(mapping)) stop("'mapping' must be a numeric vector")
    if(!length(attr(x, 'orig_levels')) == length(mapping)) stop("'mapping' must be of same length as number of levels")
  }

  type <- match.arg(type)

  if(!(is.logical(prop_log) && length(prop_log) == 1)) stop("'prop_log' must be a logical scalar")
  if(!(is.numeric(degree) && length(degree) == 1)) stop("'degree' must be a numeric scalar")

  orig_levels <- attr(x, "orig_levels")
  num_levels <- length(orig_levels)

  if(type == "non_prop") {

    if(is.null(mapping)) {
      mapping <- seq_len(num_levels)
      names(mapping) <- orig_levels

    } else if(is.null(names(mapping))) {
      stopifnot(length(mapping) == length(orig_levels))
      names(mapping) <- orig_levels

    }

  } else {

    mapping <- prop_mapping(orig_levels, prop_log)

  }

  base_level <- attr(x, "base_level")
  base_level_val <- mapping[[which(names(mapping) == base_level)]]
  mapping <- mapping - base_level_val

  orig_x <- x
  new_x <- as.numeric(mapping[as.character(x)])

  orthogonal_x <- tibble::as_tibble(round(poly(new_x, degree = degree, simple = TRUE), digits = 10))
  names(orthogonal_x) <- paste0("orthogonal_degree_", 1:degree)

  mapping <- dplyr::bind_cols(
    orig_level = x,
    actual_level = new_x,
    orthogonal_x
  ) %>%
  dplyr::group_by(orig_level) %>%
  dplyr::summarize_all(dplyr::first) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(orig_level)

  attr(x, "type") <- type
  attr(x, "mapping") <- mapping
  attr(x, "degree") <- degree
  attr(x, "prop_log") <- prop_log
  class(x) <- if(!inherits(x, "variate")) c("variate", class(x)) else class(x)

  x
}
