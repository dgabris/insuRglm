offset_term <- function(x, mapping) {
  if(!inherits(x, 'simple_factor')) stop('Please use the predictor from the dataset')

  if(inherits(x, 'custom_factor') || inherits(x, 'variate') || inherits(x, 'offset')) {
    x <- as_simple_factor(x)
  }

  if(!is.numeric(mapping)) stop("'mapping' must be a numeric vector")
  if(!length(attr(x, 'orig_levels')) == length(mapping)) stop("'mapping' must be of same length as number of levels")

  if(is.null(names(mapping))) {
    names(mapping) <- attr(x, "orig_levels")
  }

  base_level <- attr(x, "base_level")
  base_level_val <- mapping[[which(names(mapping) == base_level)]]
  mapping <- log(mapping / base_level_val)

  attr(x, "mapping") <- mapping

  class(x) <- if(!inherits(x, "offset")) c("offset", class(x)) else class(x)

  x
}
