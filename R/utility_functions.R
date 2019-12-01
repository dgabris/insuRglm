prop_mapping <- function(orig_levels, prop_log) {

  stopifnot(is.character(orig_levels))

  split_levels <- orig_levels %>%
    stringr::str_split(pattern = "-|,", simplify = TRUE)

  stopifnot(ncol(split_levels) == 2)

  left <- as.numeric(stringr::str_extract(split_levels[, 1], "[0-9\\.]+"))
  right <- as.numeric(stringr::str_extract(split_levels[, 2], "[0-9\\.]+"))
  center <- (left + right) / 2

  if(prop_log) {
    center <- log(center)
  }

  names(center) <- orig_levels
  center
}
