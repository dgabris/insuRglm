betas <- function(predictors, broom_coefs) {

  broom_coefs <- broom_coefs %>%
    dplyr::mutate(p.value = as.numeric(as.character(p.value))) %>%
    dplyr::mutate(term = stringr::str_replace_all(term, "`", "")) %>%
    dplyr::mutate(term = stringr::str_replace_all(term, c("poly\\(" = "", " , " = " ", " = [0-9]{1}\\)" = "")))

  intercept_coef <- broom_coefs %>%
    dplyr::filter(term == "(Intercept)") %>%
    dplyr::rename(factor = term) %>%
    dplyr::mutate(actual_level = "(Intercept)") %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  tidy_coefs <- broom_coefs %>%
    dplyr::filter(term != "(Intercept)") %>%
    tidyr::separate(term, into = c("factor", "actual_level"), sep = " ") %>%
    dplyr::select(
      factor, actual_level, estimate, std_error = `std.error`, statistic, p_value = `p.value`
    )

  rbind(intercept_coef, tidy_coefs) %>%
    dplyr::select(
      factor, actual_level, estimate, std_error
    ) %>%
    dplyr::mutate(std_error_pct = paste0(round(abs(100 * std_error / estimate)), "%"))
}
