prop_mapping <- function(orig_levels) {
  stopifnot(is.character(orig_levels))

  split_levels <- orig_levels %>%
    str_split(pattern = "-", simplify = TRUE)

  stopifnot(ncol(split_levels) == 2)

  left <- str_extract(split_levels[, 1], "[0-9\\.]+")
  right <- str_extract(split_levels[, 2], "[0-9\\.]+")

  center <- (as.numeric(left) + as.numeric(right)) / 2

  names(center) <- orig_levels
}
