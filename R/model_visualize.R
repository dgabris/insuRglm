#' Visualize the insuRglm model
#'
#' Visualizes the current (last) GLM model using charts which may contain observed values, fitted values and values derived
#' from model coefficients (predictions at base levels). Scale of the y-axis can be controlled using \code{y_axis} and \code{rescaled} arguments.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param factors Character scalar. One of \code{fitted}, \code{unfitted} or \code{all}.
#' @param y_axis Character scalar. Either \code{predicted} or \code{linear}.
#' @param rescaled Boolean scalar. Whether the y-axis is rescaled compared to the base level predictor at each chart.
#'
#' @return List of ggplot2 charts.
#' @export
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
#'

model_visualize <- function(setup, factors = c("fitted", "unfitted", "all"),
                            y_axis = c("predicted", "linear"), rescaled = FALSE) {

  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")
  factors <- match.arg(factors)
  y_axis <- match.arg(y_axis)
  if(!(is.logical(rescaled) && length(rescaled) == 1)) stop("'rescaled' must be a logical scalar")

  model <- setup$current_model
  predictors <- model$predictors
  relativities <- model$relativities

  if(length(predictors) != length(relativities) - 1) {
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

  plot_list <- model$factor_tables %>%
    lapply(function(x) {
      var_symbol <- rlang::sym(x$factor[[1]])
      orig_order <- x$orig_level

      x_prep <- x %>%
        dplyr::select(!!var_symbol := orig_level, weight_sum = weight,
                      dplyr::ends_with(pattern), geom_text_label) %>%
        dplyr::mutate(!!var_symbol := factor(!!var_symbol, levels = orig_order)) %>%
        purrr::set_names(stringr::str_replace(names(.), pattern, "")) %>%
        dplyr::rename(pred_base_levels = model_avg)

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
        x_prep %>%
          oneway_plot(colors = c("#33CC00", "#CC79A7", "#99FF00"), label_prefix = label_prefix)
      }

    })

  fitted_num <- length(relativities) - 1

  if(factors == "fitted") {
    plot_list[1:fitted_num]

  } else if(factors == "unfitted") {
    plot_list[-c(1:fitted_num)]

  } else {
    plot_list
  }

}
