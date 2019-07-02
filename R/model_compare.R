model_compare <- function(setup, with, type = c("1", "2", "3")) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(with %in% names(setup$ref_models))
  type <- match.arg(type)

  current_model <- setup$current_model
  current_predictors <- current_model$predictors
  current_relativities <- current_model$relativities

  if(length(current_predictors) != length(current_relativities) - 1) {
    message("Comparison won't reflect recent changes! Please run 'model_fit()' first.")
  }

  model_list <- setup$ref_models[with]
  model_list$current_model <- current_model

  intersect_predictors <- model_list %>%
    lapply(function(x) x$predictors) %>%
    purrr::reduce(base::intersect)

  result_list <- list()
  if(type == "1") {
    for(i in seq_along(intersect_predictors)) {

      predictor <- intersect_predictors[[i]]
      predictor_sym <- rlang::sym(predictor)

      base_df <- model_list$current_model$factor_tables[[i]] %>%
        dplyr::select(!!predictor_sym := orig_level, weight_sum = weight, obs_avg = obs_avg_pred_nonrescaled)

      orig_order <- base_df[[predictor]]

      for(model_nm in names(model_list)) {

        model_nm_sym <- rlang::sym(paste0(model_nm, "_fitted_avg"))

        join_df <- model_list[[model_nm]]$factor_tables[[i]] %>%
          dplyr::select(!!predictor_sym := orig_level, !!model_nm_sym := fitted_avg_pred_nonrescaled)

        base_df <- base_df %>%
          left_join(join_df, by = c(predictor))
      }

      colors <- setNames(
        c("#CC79A7", my_colors()[seq_len(length(model_list) - 1)], "#33CC00"),
        c("obs_avg", paste0(names(model_list), "_fitted_avg"))
      )

      result_list[[predictor]] <- base_df %>%
        dplyr::mutate(!!predictor_sym := factor(!!predictor_sym, levels = orig_order)) %>%
        dplyr::mutate(geom_text_label = "") %>%
        oneway_plot(label_prefix = "Predicted - ", colors = colors)
    }

  } else if(type == "2") {
    for(i in seq_along(intersect_predictors)) {

      predictor <- intersect_predictors[[i]]
      predictor_sym <- rlang::sym(predictor)

      base_df <- model_list$current_model$factor_tables[[i]] %>%
        dplyr::select(!!predictor_sym := orig_level, weight_sum = weight)

      orig_order <- base_df[[predictor]]

      for(model_nm in names(model_list)) {

        model_nm_sym <- rlang::sym(paste0(model_nm, "_pred_base_lvl"))

        join_df <- model_list[[model_nm]]$factor_tables[[i]] %>%
          dplyr::select(!!predictor_sym := orig_level, !!model_nm_sym := model_avg_lin_nonrescaled)

        base_df <- base_df %>%
          left_join(join_df, by = c(predictor))
      }

      colors <- setNames(
        c(my_colors()[seq_len(length(model_list) - 1)], "#99FF00"),
        c(paste0(names(model_list), "_pred_base_lvl"))
      )

      result_list[[predictor]] <- base_df %>%
        dplyr::mutate(!!predictor_sym := factor(!!predictor_sym, levels = orig_order)) %>%
        dplyr::mutate(geom_text_label = "") %>%
        oneway_plot(label_prefix = "Linear - ", colors = colors)
    }

  } else if(type == "3") {
    result_list <- nested_model_test(model_list)
  }

  result_list

}
