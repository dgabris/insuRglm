prop_mapping <- function(orig_levels) {

  stopifnot(is.character(orig_levels))

  split_levels <- orig_levels %>%
    stringr::str_split(pattern = "-|,", simplify = TRUE)

  stopifnot(ncol(split_levels) == 2)

  left <- stringr::str_extract(split_levels[, 1], "[0-9\\.]+")
  right <- stringr::str_extract(split_levels[, 2], "[0-9\\.]+")

  center <- (as.numeric(left) + as.numeric(right)) / 2

  names(center) <- orig_levels
  center
}

print.setup <- function(x, ...) {
  print(paste0("Target: ", x$target))
  print(paste0("Weight: ", x$weight))
  print(paste0("Actual Predictors: ", paste0(x$current_model$predictors, collapse = ", ")))
  print(paste0("Available Factors: ",
        paste0(setdiff(x$simple_factors, x$current_model$predictors), collapse = ", "))
  )
}
