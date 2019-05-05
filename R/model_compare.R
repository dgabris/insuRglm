model_compare <- function(setup, with, type = c("factors", "tests")) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(with %in% names(setup$ref_models))
  type <- match.arg(type)

  current_model <- setup$current_model
  current_predictors <- current_model$predictors
  current_relativities <- current_model$relativities

  if(length(current_predictors) != length(current_relativities)) {
    message("Comparison won't reflect recent changes! Please run 'model_fit()' first.")
  }

  current_factors <- current_model$factor_tables

  ref_model <- setup$ref_models[[with]]
  ref_factors <- ref_model$factor_tables

  purrr::pmap(list(current_factors, ref_factors), function(x, y) {
    stopifnot(x$factor[[1]] == y$factor[[1]])
    var_name <- x$factor[[1]]
    var_symbol <- rlang::sym(var_name)

    stopifnot(all(x$orig_level == y$orig_level))
    orig_order <- x$orig_level

    x <- x %>%
      dplyr::select(!!var_symbol := orig_level, weight_sum = weight, current_model_avg = model_avg_lin_rescaled)

    y <- y %>%
      dplyr::select(!!var_symbol := orig_level, ref_model_avg = model_avg_lin_rescaled)

    x %>%
      dplyr::left_join(y, by = c(var_name)) %>%
      dplyr::mutate(!!var_symbol := factor(!!var_symbol, levels = orig_order)) %>%
      oneway_plot()
  })

}
