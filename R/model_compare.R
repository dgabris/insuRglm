#' Compare two or more insuRglm models
#'
#' Compares multiple insuRglm models with the current (last) model. The comparison models must be saved using \code{model_save}.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param type Character scalar. One of 'nested_model_test' or 'rmse'.
#' 'nested_model_test' will produce nested model test in form of matrix of standard error percentages.
#' 'rmse' will produce a plot comparing RMSE across all models (also separately for train and CV if \code{model_crossval} was used)
#'
#' @return Either a matrix-like dataframe of nested model test results or a ggplot2 chart.
#' @export
#'
#' @seealso \code{\link{model_save}}, \code{\link{model_crossval}}
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
#' # nested model test of model with and without the agecat
#' modeling %>%
#'   model_compare(type = 'nested_model_test')
#'
#' # compare training RMSE on all models so far
#' modeling %>%
#'   model_compare(type = 'rmse')
#'
#' # compare both training and CV RMSE on all models
#' modeling_with_cv <- modeling %>%
#'   model_crossval(cv_folds = 10, stratified = TRUE)
#'
#' modeling_with_cv %>%
#'   model_compare(type = 'rmse')
#'

model_compare <- function(setup, type = c("rmse", "nested_model_test"), buckets = 10) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")

  type <- match.arg(type)

  if(!is.null(buckets)) stopifnot(is.integer(buckets) || is.numeric(buckets))

  if(inherits(setup$current_model, "unfitted_model")) {
    message("Comparison won't reflect recent changes! Please run 'model_fit()' first.")
  }

  model_list <- setup$ref_models
  model_list$current_model <- setup$current_model

  if(type == "nested_model_test") {
    result <- nested_model_test(model_list)

  } else if(type == "rmse") {
    train <- setup$data_train
    metric <- type
    metric_sym <- rlang::sym(metric)

    actual <- train[[setup$target]]
    weight_vector <- train[[setup$weight]]

    metric_df <- tibble::tibble()
    for(model_nm in names(model_list)) {
      model <- model_list[[model_nm]]
      expected_train <- model$train_predictions
      expected_cv <- model$cv_predictions

      if(!is.null(buckets)) {
        train_df <- lift_buckets(actual, expected_train, weight_vector, buckets, weighted = TRUE) %>%
          dplyr::select(-bucket)

      } else {
        train_df <- tibble::tibble(actual = actual, expected = expected_train, weight = weight_vector)
      }

      train_metric <- comparison_metric(train_df, metric)
      train_metric_df <- tibble::tibble(model = model_nm, data = "train", !!metric_sym := train_metric)

      if(!is.null(expected_cv)) {
        if(!is.null(buckets)) {
          cv_df <- lift_buckets(actual, expected_cv, weight_vector, buckets, weighted = TRUE) %>%
            dplyr::select(-bucket)

        } else {
          cv_df <- tibble::tibble(actual = actual, expected = expected_cv, weight = weight_vector)
        }

        cv_metric <- comparison_metric(cv_df, metric)
        cv_metric_df <- tibble::tibble(model = model_nm, data = "crossval", !!metric_sym := cv_metric)

      } else {
        cv_metric_df <- NULL
      }

      temp_df <- dplyr::bind_rows(train_metric_df, cv_metric_df)
      metric_df <- dplyr::bind_rows(metric_df, temp_df)
    }

    g <- metric_df %>%
      dplyr::mutate(model = factor(model, levels = names(model_list))) %>%
      ggplot2::ggplot(ggplot2::aes(x = model, y = !!metric_sym, group = data)) +
      ggplot2::geom_line(ggplot2::aes(color = data)) +
      ggplot2::geom_point(ggplot2::aes(color = data)) +
      ggplot2::labs(x = "Model", y = metric, color = "Data") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.45),
        plot.subtitle = ggplot2::element_text(hjust = 0.45),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    if(!is.null(buckets)) {
      g <- g +
        ggplot2::ggtitle(
          label = "Model Comparison",
          subtitle = paste0(buckets, " buckets created prior the '", metric, "' calculation")
        )
    } else {
      g <- g +
        ggplot2::ggtitle("Model Comparison")
    }

    result <- g
  }

  result
}
