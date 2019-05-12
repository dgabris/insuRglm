betas <- function(predictors, broom_coefs) {
  predictors_regex <- paste0(predictors, collapse = "|")

  intercept_coef <- broom_coefs %>%
    dplyr::filter(term == "(Intercept)") %>%
    dplyr::rename(factor = term) %>%
    dplyr::mutate(actual_level = "(Intercept)") %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  simple_coefs <- broom_coefs %>%
    dplyr::filter(term != "(Intercept)" & !stringr::str_detect(term, ":")) %>%
    dplyr::mutate(factor = stringr::str_extract(term, predictors_regex)) %>%
    dplyr::mutate(actual_level = stringr::str_replace(term, factor, "")) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  interaction_predictors <- unlist(predictors[stringr::str_detect(predictors, "\\*")])
  if(length(interaction_predictors) > 0) {
    interaction_coefs <- interaction_coefs(broom_coefs, predictors_regex, interaction_predictors)
  } else {
    interaction_coefs <- NULL
  }

  rbind(intercept_coef, simple_coefs, interaction_coefs) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error
    ) %>%
    dplyr::mutate(std_error_pct = paste0(round(abs(100 * std_error / estimate)), "%"))
}
