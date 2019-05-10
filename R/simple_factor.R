simple_factor <- function(x, exposure, orig_levels) {
  stopifnot(inherits(x, "factor"))
  stopifnot(length(x) == length(exposure))
  stopifnot(is.numeric(exposure))
  stopifnot(all(!is.na(exposure)))

  sorted_by_exposure <- names(sort(tapply(exposure, x, sum), decreasing = TRUE))
  model_base_level <- sorted_by_exposure[[1]]
  model_base_level_ind <- which(orig_levels == model_base_level)
  model_levels <- c(model_base_level, orig_levels[-model_base_level_ind])

  levels(x) <- orig_levels
  attr(x, "orig_levels") <- orig_levels

  attr(x, "model_levels") <- model_levels

  class(x) <- c("simple_factor", class(x))

  x
}
