interaction_coefs <- function(broom_coefs, all_predictors_regex, interaction_predictors) {

  stopifnot(inherits(broom_coefs, "data.frame"))
  stopifnot(is.character(interaction_predictors))

  interaction_coefs_prep <- broom_coefs %>%
    dplyr::filter(term != "(Intercept)" & stringr::str_detect(term, ":")) %>%
    dplyr::mutate(term_split = stringr::str_split(term, ":")) %>%
    dplyr::mutate(factor_names = stringr::str_extract_all(term_split, all_predictors_regex))

  interaction_coefs <- tibble()
  for(predictor in interaction_predictors) {
    vars <- unlist(stringr::str_split(predictor, "\\*"))

    interaction_coefs <- interaction_coefs_prep %>%
      dplyr::filter(vapply(factor_names, function(x) identical(intersect(x, vars), x), logical(1))) %>%
      dplyr::mutate(
        term_split = purrr::map2(term_split, factor_names, function(x, y) x[match(y, vars)])
      ) %>%
      dplyr::mutate(factor_names = stringr::str_extract_all(term_split, all_predictors_regex)) %>%
      dplyr::bind_rows(interaction_coefs, .)
  }

  interaction_coefs %>%
    dplyr::mutate(factor_regex = vapply(factor_names, function(x) paste(x, collapse = "|"), character(1))) %>%
    dplyr::mutate(
      actual_level = purrr::map2(term_split, factor_regex, function(x, y) stringr::str_replace_all(x, y, ""))
    ) %>%
    dplyr::mutate(actual_level = vapply(actual_level, function(x) paste(x, collapse = "."), character(1))) %>%
    dplyr::mutate(factor = stringr::str_replace(factor_regex, "\\|", "\\*")) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

}
