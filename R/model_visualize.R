model_visualize <- function(setup, y_axis = c("predicted", "linear"), rescaled = FALSE) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  y_axis <- match.arg(y_axis)

  model <- setup$current_model
  predictors <- model$predictors
  relativities <- model$relativities

  if(length(predictors) != length(relativities)) {
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

  model$factor_tables %>%
    lapply(function(x) {
      var_symbol <- rlang::sym(x$factor[[1]])
      orig_order <- x$orig_level

      x <- x %>%
        dplyr::select(!!var_symbol := orig_level, weight_sum = weight,
                      dplyr::ends_with(pattern), geom_text_label) %>%
        dplyr::mutate(!!var_symbol := factor(!!var_symbol, levels = orig_order)) %>%
        purrr::set_names(stringr::str_replace(names(.), pattern, "")) %>%
        dplyr::rename(pred_base_levels = model_avg) %>%
        oneway_plot(colors = c("#33CC00", "#CC79A7", "#99FF00"), label_prefix = label_prefix)
    })
}
