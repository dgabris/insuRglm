custom_factor <- function(x, mapping) {
  stopifnot(inherits(x, "simple_factor"))
  stopifnot(is.numeric(mapping) || is.integer(mapping))

  if(is.null(names(mapping))) {
    names(mapping) <- orig_levels
  }

  attr(x, "mapping") <- mapping
  class(x) <- c("custom_factor", class(x))

  x
}
