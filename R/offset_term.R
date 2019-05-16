offset_term <- function(x, mapping) {

  stopifnot(inherits(x, "simple_factor"))
  stopifnot(is.numeric(mapping) || is.integer(mapping))
  stopifnot(length(attr(x, "orig_levels")) == length(mapping))

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
