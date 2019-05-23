crossval_predict <- function(data_train, model, cv_folds, stratified) {
  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(inherits(model, "fitted_model"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  target <- model$target
  target_sym <- rlang::sym(target)
  predictors <- model$predictors
  predictors_collapsed <- paste0(predictors, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors_collapsed))
  family <- model$family
  weight <- model$weight

  train <- remap_predictors(list(train = data_train), predictors)$train %>%
    mutate(id = row_number())

  n_losses <- nrow(dplyr::filter(data_train, !!target_sym > 0))
  n_non_losses <- nrow(dplyr::filter(data_train, !!target_sym == 0))
  stopifnot((n_losses + n_non_losses) == nrow(data_train))

  if(stratified && (n_non_losses >= cv_folds)) {
    train <- train %>%
      dplyr::mutate(cv_fold =
        if_else(
          !!target_sym > 0,
          sample(1:cv_folds, n_losses, replace = TRUE),
          sample(1:cv_folds, n_non_losses, replace = TRUE)
        )
      )

  } else {
    train <- train %>%
      dplyr::mutate(cv_fold = sample(1:cv_folds, nrow(.), replace = TRUE))
  }

  cv_predictions <- tibble::tibble()
  for(fold in seq_len(cv_folds)) {

    fold_train <- train %>%
      dplyr::filter(cv_fold != fold)

    fold_test <- train %>%
      dplyr::filter(cv_fold == fold)

    glm <- glm(
      formula = formula,
      family = family,
      weights = fold_train[[weight]],
      data = fold_train
    )

    fold_predictions <- dplyr::bind_cols(
      id = fold_test$id,
      cv_pred_target = predict(glm, newdata = fold_test, type = "response")
    )

    cv_predictions <- dplyr::bind_rows(cv_predictions, fold_predictions)
  }

  cv_predictions %>%
    dplyr::arrange(id) %>%
    dplyr::select(cv_pred_target) %>%
    unlist(use.names = FALSE)
}
