relativities <- function(factor_tables, betas) {

  base_value <- betas %>%
    dplyr::filter(factor == "(Intercept)") %>%
    dplyr::mutate(estimate = exp(estimate)) %>%
    dplyr::select(estimate) %>%
    unlist(use.names = FALSE)

  base_value_list <- list(`0 - base_value` = base_value)

  relativity_list <- Filter(factor_tables, f = function(x) {
    all(!is.na(x$model_avg_pred_rescaled))
  }) %>%
  lapply(function(x) {
    factor_nm <- x$factor[[1]]
    factor_nm_sym <- rlang::sym(factor_nm)

    x %>%
      dplyr::select(!!factor_nm_sym := orig_level, relativity = model_avg_pred_rescaled, weight)
  })

  c(base_value_list, relativity_list)
}
