simple_factor <- function(x, exposure) {
  stopifnot(inherits(x, "factor"))
  stopifnot(length(x) == length(exposure))
  stopifnot(is.numeric(exposure))
  stopifnot(all(!is.na(exposure)))

  orig_levels <- levels(x)

  sorted_by_exposure <- sort(tapply(exposure, x, sum), decreasing = TRUE)

  attr(x, "orig_levels") <- orig_levels
  attr(x, "model_levels") <- sorted_by_exposure

  class(x) <- c("simple_factor", class(x))

  x
}
