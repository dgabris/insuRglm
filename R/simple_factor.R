simple_factor <- function(x, orig_levels, base_level) {
  stopifnot(inherits(x, "factor"))
  stopifnot(is.character(orig_levels))
  stopifnot(is.character(base_level) && length(base_level) == 1)

  x <- factor(as.character(x), levels = orig_levels)
  attr(x, "orig_levels") <- orig_levels
  attr(x, "base_level") <- base_level

  class(x) <- c("simple_factor", class(x))

  x
}
