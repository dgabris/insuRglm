#' Visualize the insuRglm model
#'
#' Visualizes the current (last) GLM model using charts which may contain observed values, fitted values and values derived
#' from model coefficients (predictions at base levels). Scale of the y-axis can be controlled using \code{y_axis} and \code{rescaled} arguments.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param factors Character scalar/vector. Either one of \code{fitted}, \code{unfitted}, \code{all} or
#' a name of one or multiple currently fitted model predictors.
#' @param by Character scalar. A name of one of currently fitted predictors in the model.
#' Will result in two-way chart showing the combination of the main effects (without interaction).
#' @param y_axis Character scalar. Either \code{predicted} or \code{linear}.
#' @param rescaled Boolean scalar. Whether the y-axis is rescaled compared to the base level predictor at each chart.
#' @param ref_models Character vector. Names of one or multiple reference models created by using \code{model_save}
#'
#' @return List of ggplot2 charts.
#' @export
#' @import patchwork
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
#'   factor_add(agecat) %>%
#'   model_fit()
#'
#' # this is also the default
#' modeling %>%
#'   model_visualize(factors = 'fitted', y_axis = 'predicted', rescaled = FALSE)
#'
#' modeling %>%
#'   model_visualize(factors = 'fitted', y_axis = 'linear', rescaled = TRUE)
#'
#' modeling %>%
#'   model_visualize(factors = 'unfitted')
#'
#' modeling %>%
#'   model_visualize(factors = c('pol_yr', 'agecat'))
#'
#' modeling <- modeling %>%
#'   factor_add(gender) %>%
#'   model_fit()
#'
#' modeling %>%
#'   model_visualize(factors = 'fitted', by = 'gender')
#'
#' modeling %>%
#'   model_visualize(factors = 'agecat', by = 'gender', y_axis = 'linear')
#'
#' modeling <- modeling %>%
#'   model_save('model1') %>%
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = 1:6, degree = 2)) %>%
#'   model_fit()
#'
#' modeling %>%
#'   model_visualize(ref_models = "model1")
#'
#' modeling %>%
#'   model_visualize(y_axis = "linear", ref_models = "model1")
#'

model_visualize <- function(setup, factors = "fitted", by = NULL, y_axis = c("predicted", "linear"), rescaled = FALSE,
                            ref_models = NULL) {

  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")
  y_axis <- match.arg(y_axis)
  if(!(is.logical(rescaled) && length(rescaled) == 1)) stop("'rescaled' must be a logical scalar")

  if(!factors %in% c("fitted", "unfitted", "all") && !factors %in% setup$simple_factors) {
    stop(paste0(
      "'factors' must be either one of 'fitted', 'unfitted', 'all'",
      " or a name of one or multiple currently fitted model predictors."))
  }

  model <- setup$current_model
  predictors <- model$predictors

  if(inherits(model, "unfitted_model")) {
    message("Visualization won't reflect recent changes! Please run 'model_fit()' first.")
  }

  if(y_axis == "predicted" && rescaled) {
    pattern <- "_pred_rescaled"
    label_prefix <- "Predicted Rescaled - "
  } else if(y_axis == "predicted" && !rescaled) {
    pattern <- "_pred_nonrescaled"
    label_prefix <- "Predicted - "
  } else if(y_axis == "linear" && rescaled) {
    pattern <- "_lin_rescaled"
    label_prefix <- "Linear Rescaled - "
  } else if(y_axis == "linear" && !rescaled) {
    pattern <- "_lin_nonrescaled"
    label_prefix <- "Linear - "
  }

  if(!is.null(by)) {
    if(!by %in% predictors) stop("'by' must be a name of one of currently fitted model predictors.")

    if(length(factors) == 1) {
      if(factors %in% c("unfitted", "all")) {
        message("Two-way charts will be created only for fitted factors.")
        factors <- setdiff(predictors, by)
      } else if(factors == "fitted") {
        factors <- setdiff(predictors, by)
      }
    } else if(any(!factors %in% predictors)) {
      message("Two-way charts will be created only for fitted factors.")
      factors <- intersect(factors, predictors)
      if(length(factors) == 0) stop("'factors' doesn't contain any fitted factors, no visualization can be produced.")
    }

    train <- setup$data_train
    predictions <- model$train_predictions
    weights <- train[[setup$weight]]
    current_baseline <- model$current_baseline
    by_sym <- rlang::sym(by)

    tables <- lapply(factors, function(x) {

      obs_avg <- compute_obs_avg(train[[x]], by = train[[by]], train[[setup$target]], weights)
      fitted_avg <- compute_fitted_avg(train[[x]], by = train[[by]], predictions, weights)
      x_betas <- model$betas %>% dplyr::filter(factor %in% c("(Intercept)", x))
      by_betas <- model$betas %>% dplyr::filter(factor %in% c("(Intercept)", by))
      model_avg <- compute_model_avg(train[[x]], x_betas, current_baseline, train[[by]], by_betas)

      if(y_axis == "predicted") {
        label_prefix <- "Two way actual vs expected - "
        label_suffix <- paste0(" x ", by)

        obs_avg %>%
          dplyr::left_join(fitted_avg, by = c("orig_level", "by")) %>%
          dplyr::select(orig_level, by, weight, dplyr::contains(pattern)) %>%
          setNames(stringr::str_replace(names(.), pattern, "")) %>%
          dplyr::rename(`Observed Average` = obs_avg, `Fitted Average` = fitted_avg) %>%
          dplyr::mutate(label_prefix = label_prefix, label_suffix = label_suffix)
      } else if(y_axis == "linear") {
        label_prefix <- "Two way model parameters - "
        label_suffix <- paste0(" x ", by)

        obs_avg %>%
          dplyr::left_join(model_avg, by = c("orig_level", "by")) %>%
          dplyr::select(orig_level, by, weight, dplyr::contains(pattern)) %>%
          dplyr::select(-dplyr::starts_with("obs_avg")) %>%
          setNames(stringr::str_replace(names(.), pattern, "")) %>%
          dplyr::rename(`Model Parameters` = model_avg) %>%
          dplyr::mutate(label_prefix = label_prefix, label_suffix = label_suffix)
      }
    }) %>%
    setNames(factors)

    plot_list <- lapply(factors, function(x) {
      factor_sym <- rlang::sym(x)

      label_prefix <- tables[[x]]$label_prefix[[1]]
      label_suffix <- tables[[x]]$label_suffix[[1]]

      tables[[x]] %>%
        dplyr::rename(!!factor_sym := orig_level, !!by_sym := by, weight_sum = weight) %>%
        dplyr::select(-label_prefix, -label_suffix) %>%
        twoway_plot(label_prefix = label_prefix, label_suffix = label_suffix)
    }) %>%
    setNames(factors)

    return(plot_list)
  }

  if(!is.null(ref_models)) {
    if(!(is.character(ref_models))) stop("'ref_models' must be of type character")
    if(any(!ref_models %in% names(setup$ref_models))) stop("One of the model names is invalid")

    model_list <- setup$ref_models[ref_models]
    model_list$current_model <- model

    intersect_fitted <- model_list %>%
      lapply(function(x) x$predictors) %>%
      purrr::reduce(base::intersect)

    intersect_unfitted <- model_list %>%
      lapply(function(x) setdiff(names(x$factor_tables), x$predictors)) %>%
      purrr::reduce(base::intersect)

    if(length(factors) == 1) {
      if(factors == "fitted") {
        factors <- intersect_fitted
      } else if(factors == "unfitted") {
        if(y_axis == "predicted") factors <- intersect_unfitted
        if(y_axis == "linear") {
          factors <- intersect_fitted
          message("When using `ref_models` and `y_axis = 'linear'`, only fitted factors are plotted.")
        }
      } else if(factors == "all") {
        if(y_axis == "predicted") factors <- c(intersect_fitted, intersect_unfitted)
        if(y_axis == "linear") {
          factors <- intersect_fitted
          message("When using `ref_models` and `y_axis = 'linear'`, only fitted factors are plotted.")
        }
      }
    } else {
      factors <- base::intersect(factors, c(intersect_fitted, intersect_unfitted))
    }

    plot_list <- list()
    for(i in seq_along(factors)) {

      predictor <- factors[[i]]
      predictor_sym <- rlang::sym(predictor)

      base_df <- model_list$current_model$factor_tables[[predictor]] %>%
        dplyr::select(!!predictor_sym := orig_level, weight_sum = weight, dplyr::contains(pattern)) %>%
        dplyr::select(-dplyr::starts_with("model_avg"), -dplyr::starts_with("fitted_avg")) %>%
        setNames(stringr::str_replace(names(.), pattern, "")) %>%
        dplyr::rename(`Observed Average` = obs_avg)

      orig_order <- base_df[[predictor]]

      for(model_nm in names(model_list)) {

        model_nm_fitted_sym <- rlang::sym(paste0(model_nm, "_fitted_avg"))
        model_nm_model_sym <- rlang::sym(paste0(model_nm, "_model_avg"))

        join_df <- model_list[[model_nm]]$factor_tables[[predictor]] %>%
          dplyr::select(!!predictor_sym := orig_level, dplyr::contains(pattern))

        if(y_axis == "predicted") {
          join_df <- join_df %>%
            dplyr::select(-dplyr::starts_with("obs_avg"), -dplyr::starts_with("model_avg"))
        }

        if(y_axis == "linear") {
          join_df <- join_df %>%
            dplyr::select(-dplyr::starts_with("obs_avg"), -dplyr::starts_with("fitted_avg"))
        }

        col_nms <- names(join_df) %>%
          stringr::str_replace(pattern, "") %>%
          stringr::str_replace("fitted_avg", "Fitted Average") %>%
          stringr::str_replace("model_avg", "Model Parameters")

        names(join_df) <- c(col_nms[[1]], paste0(col_nms[-1], " (", model_nm, ")"))

        base_df <- base_df %>%
          dplyr::left_join(join_df, by = c(predictor))
      }

      if(y_axis == "predicted") {
        colors <- setNames(
          c("#CC79A7", my_colors()[seq_len(length(model_list) - 1)], "#33CC00"),
          c("Observed Average", paste0("Fitted Average (", names(model_list), ")"))
        )
      } else if(y_axis == "linear") {
        base_df <- base_df %>%
          dplyr::select(-`Observed Average`)

        colors <- setNames(
          c(my_colors()[seq_len(length(model_list) - 1)], "#99FF00"),
          paste0("Model Parameters (", names(model_list), ")")
        )
      }

      plot_list[[predictor]] <- base_df %>%
        dplyr::mutate(!!predictor_sym := factor(!!predictor_sym, levels = orig_order)) %>%
        dplyr::mutate(geom_text_label = "") %>%
        oneway_plot(label_prefix = label_prefix, colors = colors)
    }

    return(plot_list)
  }

  if(length(factors) == 1) {
    if(factors == "fitted") {
      tables <- model$factor_tables[predictors]
    } else if(factors == "unfitted") {
      tables <- model$factor_tables[setdiff(setup$simple_factors, predictors)]
    } else if(factors == "all") {
      tables <- model$factor_tables
    }
  } else {
    tables <- model$factor_tables[factors]
  }

  plot_list <- tables %>%
    lapply(function(x) {
      var_symbol <- rlang::sym(x$factor[[1]])
      orig_order <- x$orig_level

      x_prep <- x %>%
        dplyr::select(!!var_symbol := orig_level, weight_sum = weight,
                      dplyr::ends_with(pattern), geom_text_label) %>%
        dplyr::mutate(!!var_symbol := factor(!!var_symbol, levels = orig_order)) %>%
        purrr::set_names(stringr::str_replace(names(.), pattern, "")) %>%
        dplyr::rename(
          `Observed Average` = obs_avg,
          `Fitted Average` = fitted_avg,
          `Model Parameters` = model_avg
        )

      x_name <- names(x_prep)[[1]]

      if(stringr::str_detect(x_name, "\\*")) {

      x_sym <- rlang::sym(x_name)
      main_vars <- stringr::str_split(x_name, "\\*", simplify = TRUE)
      main_vars[[1]] <- x_name

        x_prep %>%
          dplyr::select(-obs_avg, -fitted_avg, -geom_text_label) %>%
          tidyr::separate(!!x_sym, into = main_vars) %>%
          twoway_plot(label_prefix = label_prefix)
      } else {
        colors <- c(
          "Observed Average" = "#CC79A7",
          "Fitted Average" = "#33CC00",
          "Model Parameters" = "#99FF00"
        )

        x_prep %>%
          oneway_plot(colors = colors, label_prefix = label_prefix)
      }

    })

  plot_list

}
