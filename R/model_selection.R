model_selection <- function(setup, data = c("train", "crossval"), metric = c("rmse"), buckets = NULL) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  data <- match.arg(data)
  metric <- match.arg(metric)
  if(!is.null(buckets)) stopifnot(is.integer(buckets) || is.numeric(buckets))

  if(inherits(setup, "offset_model")) {
    message("It doesn't make sense to do model selection at this point.")
    return(setup)
  }

  if(data == "crossval" && is.null(setup$current_model$cv_predictions)) {
    message("No CV predictions found. Please run 'model_crossval()' first.")
    return(setup)
  }

  metric_sym <- rlang::sym(metric)
  train <- setup$data_train

  actual <- train[[setup$target]]
  weight_vector <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]

  model_list <- list()
  for(ref_model_nm in names(setup$ref_models)) {
    model_list[[ref_model_nm]] <- setup$ref_models[[ref_model_nm]]
  }

  model_list$current_model <- setup$current_model

  metric_df <- tibble::tibble()

  for(model_nm in names(model_list)) {
    model <- model_list[[model_nm]]
    expected_train <- model$train_predictions
    expected_cv <- model$cv_predictions

    if(!is.null(buckets)) {
      train_df <- lift_buckets(actual, expected_train, weight_vector, buckets) %>%
        dplyr::select(-bucket)

    } else {
      train_df <- tibble::tibble(actual = actual, expected = expected_train, weight = weight_vector)
    }

    if(data == "crossval") {
      if(!is.null(buckets)) {
        cv_df <- lift_buckets(actual, expected_cv, weight_vector, buckets) %>%
          dplyr::select(-bucket)

      } else {
        cv_df <- tibble::tibble(actual = actual, expected = expected_cv, weight = weight_vector)
      }

    } else {
      cv_df <- tibble::tibble(actual = numeric(0), expected = numeric(0), weight = numeric(0))
    }

    train_metric <- selection_metric(train_df, metric)
    cv_metric <- if(data == "crossval") selection_metric(cv_df, metric) else NULL

    temp_df <- dplyr::bind_rows(
      tibble::tibble(model = model_nm, data = "train", !!metric_sym := train_metric),
      if(data == "crossval") tibble::tibble(model = model_nm, data = "crossval", !!metric_sym := cv_metric) else NULL
    )

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
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if(!is.null(buckets)) {
    g +
      ggplot2::ggtitle(
        label = "Model Selection",
        subtitle = paste0(buckets, " buckets created prior the '", metric, "' calculation")
      )
  } else {
    g +
      ggplot2::ggtitle("Model Selection")
  }

}
