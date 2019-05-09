variate <- function(x, type = c("prop", "non_prop"), mapping = NULL) {

  stopifnot(inherits(x, "simple_factor"))
  type <- match.arg(type)
  if(!is.null(mapping)) stopifnot(is.numeric(mapping) || is.integer(mapping))

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

    mapping <- prop_mapping(orig_levels)

  }

  base_level <- attr(x, "model_levels")[[1]]
  base_level_val <- mapping[[which(names(mapping) == base_level)]]
  mapping <- mapping - base_level_val

  attr(x, "mapping") <- mapping
  class(x) <- c("variate", class(x))

  x
}
