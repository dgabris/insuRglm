model_selection <- function(setup, method = c("train", "crossval"),
                            cv_folds = 10, cv_stratified = FALSE, buckets = 10) {

  stopifnot(inherits(setup, "setup"))
  method <- match.arg(method)
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  model_list <- list()
  for(ref_model_nm in names(setup$ref_models)) {
    model_list[[ref_model_nm]] <- setup$ref_models[[ref_model_nm]]
  }

  model_list$current_model <- setup$current_model

  lift_buckets_list_train <- lift_buckets(setup, model_list, buckets, method = "train", cv_folds, cv_stratified)
  lift_buckets_list_cv <- if(method == "crossval") {
    lift_buckets(setup, model_list, buckets, method = "crossval", cv_folds, cv_stratified)
  } else {
    NULL
  }

  model_names <- names(model_list)
  rmse_train_vector <- vector("numeric", length(model_names))
  rmse_cv_vector <- if(method == "crossval") vector("numeric", length(model_names)) else NULL

  for(i in seq_along(model_names)) {

    rmse_train <- lift_buckets_list_train[[i]] %>%
      tidyr::spread(key = type, value = target) %>%
      dplyr::mutate(squared_residual = (actual - expected)^2) %>%
      dplyr::summarize(rmse = sqrt(sum(squared_residual) / n())) %>%
      unlist(use.names = FALSE)

    if(method == "crossval") {
      rmse_cv <- lift_buckets_list_cv[[i]] %>%
        tidyr::spread(key = type, value = target) %>%
        dplyr::mutate(squared_residual = (actual - expected)^2) %>%
        dplyr::summarize(rmse = sqrt(sum(squared_residual) / n())) %>%
        unlist(use.names = FALSE)
    } else {
      rmse_cv <- NULL
    }

    rmse_train_vector[[i]] <- rmse_train
    rmse_cv_vector[[i]] <- rmse_cv
  }

  train_tibble <- tibble::tibble(model = model_names, method = "train", rmse = rmse_train_vector)
  if(method == "crossval") {
    cv_tibble <- tibble::tibble(model = model_names, method = "crossval", rmse = rmse_cv_vector)
  } else {
    cv_tibble <- tibble::tibble()
  }

  dplyr::bind_rows(
    train_tibble,
    cv_tibble
  ) %>%
  dplyr::mutate(model = factor(model, levels = model_names)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model, y = rmse, group = method)) +
    ggplot2::geom_line(ggplot2::aes(color = method)) +
    ggplot2::geom_point(ggplot2::aes(color = method)) +
    ggplot2::labs(x = "Model", y = "RMSE", color = "Method") +
    ggplot2::ggtitle(paste0("Model Selection - ", buckets, " buckets")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

}
