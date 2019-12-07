#' Compare two or more insuRglm models
#'
#' Compares multiple insuRglm models with the current (last) model. The comparison models must be saved using \code{model_save}.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param with Character vector. Names of models that should be compared against the current model.
#' @param type Character scalar. One of '1', '2' or '3'.
#' '1' will produce visual comparison of observed values versus fitted values of each comparison model.
#' '2' will produce visual comparison of predictions at base levels of each comparison model.
#' '3' will produce nested model test in form of matrix of standard error percentages.
#'
#' @return Either list of ggplot2 charts (types '1', '2') or matrix-like dataframe of nested model test results (type '3').
#' @export
#'
#' @seealso \code{\link{model_save}}
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = sev_train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   model_fit() %>%
#'   model_save('model1') %>%
#'   factor_add(agecat) %>%
#'   model_fit() %>%
#'   model_save('model2') %>%
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) %>%
#'   model_fit()
#'
#' # compare observed versus fitted
#' modeling %>%
#'   model_compare(with = 'model2', type = '1')
#'
#' # compare predictions at base levels
#' modeling %>%
#'   model_compare(with = 'model2', type = '2')
#'
#' # nested model test of model with and without the agecat
#' modeling %>%
#'   model_compare(with = 'model1', type = '3')
#'

model_compare <- function(setup, with, type = c("1", "2", "3")) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")
  if(!(is.character(with) && length(with) == 1)) stop("'with' must be a character scalar")
  if(!with %in% names(setup$ref_models)) stop('Invalid model name provided')

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
          dplyr::left_join(join_df, by = c(predictor))
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
          dplyr::left_join(join_df, by = c(predictor))
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
