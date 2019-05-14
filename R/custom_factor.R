custom_factor <- function(x, mapping) {
  stopifnot(inherits(x, "simple_factor"))
  stopifnot(is.numeric(mapping) || is.integer(mapping))
  stopifnot(length(levels(x)) == length(mapping))

  if(is.null(names(mapping))) {
    names(mapping) <- attr(x, "orig_levels")
  }

  attr(x, "mapping") <- mapping
  class(x) <- if(!inherits(x, "custom_factor")) c("custom_factor", class(x)) else class(x)

  x
}
