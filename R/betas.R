betas <- function(predictors, broom_coefs) {
  predictors_regex <- paste0(predictors, collapse = "|")

  intercept_row <- broom_coefs %>%
    dplyr::filter(term == "(Intercept)") %>%
    dplyr::rename(factor = term) %>%
    dplyr::mutate(actual_level = "(Intercept)") %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  tidy_coefs <- broom_coefs %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(factor = stringr::str_extract(term, predictors_regex)) %>%
    dplyr::mutate(actual_level = stringr::str_replace(term, factor, "")) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  rbind(intercept_row, tidy_coefs) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error
    ) %>%
    dplyr::mutate(std_error_pct = paste0(round(abs(100 * std_error / estimate)), "%"))
}
