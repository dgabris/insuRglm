crossval_predict <- function(data_train, model, cv_folds) {
  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(inherits(model, "fitted_model"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  target <- model$target
  predictors <- model$predictors
  predictors_collapsed <- paste0(predictors, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors_collapsed))
  family <- model$family
  weight <- model$weight

  train <- remap_predictors(list(train = data_train), predictors)$train %>%
    dplyr::mutate(cv_fold = sample(1:cv_folds, nrow(.), replace = TRUE)) %>%
    dplyr::mutate(id = row_number())

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
