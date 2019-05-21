variate <- function(x, type = c("prop", "non_prop"), prop_log = TRUE,  mapping = NULL, degree = 1) {

  stopifnot(inherits(x, "simple_factor"))
  type <- match.arg(type)
  if(!is.null(mapping)) stopifnot(is.numeric(mapping) || is.integer(mapping))
  if(!is.null(mapping)) stopifnot(length(attr(x, "orig_levels")) == length(mapping))

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
  dplyr::distinct() %>%
  dplyr::arrange(orig_level)

  attr(x, "mapping") <- mapping
  attr(x, "degree") <- degree
  class(x) <- if(!inherits(x, "variate")) c("variate", class(x)) else class(x)

  x
}
