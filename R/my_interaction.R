my_interaction <- function(vars_list) {

  stopifnot(typeof(vars_list) == "list")

  base_levels <- vapply(vars_list, function(x) attr(x, "base_level"), character(1))

  base_levels_for_regex <- base_levels %>%
    stringr::str_replace_all("\\|", "\\\\|") %>%
    stringr::str_replace_all("\\(", "\\\\(") %>%
    stringr::str_replace_all("\\)", "\\\\)") %>%
    stringr::str_replace_all("\\[", "\\\\[") %>%
    stringr::str_replace_all("\\]", "\\\\]") %>%
    stringr::str_replace_all("\\.", "\\\\.")

  base_levels_regex <- paste(base_levels_for_regex, collapse = "|")

  new_base_level <- paste(base_levels, collapse = ".")

  x <- interaction(vars_list)

  orig_levels <- levels(x)
  new_levels <- dplyr::if_else(stringr::str_detect(orig_levels, base_levels_regex), new_base_level, orig_levels)

  mapping <- setNames(new_levels, orig_levels)

  attr(x, "orig_levels") <- orig_levels
  attr(x, "mapping") <- mapping
  attr(x, "base_level") <- new_base_level

  class(x) <- c("interaction", class(x))

  x
}
