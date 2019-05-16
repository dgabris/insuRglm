as_simple_factor <- function(x) {
  stopifnot(inherits(x, "simple_factor"))

  attr(x, "mapping") <- NULL
  class(x) <- setdiff(class(x), c("variate", "custom_factor", "offset"))

  x
}
