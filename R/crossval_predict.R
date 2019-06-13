crossval_predict <- function(data_train, model, cv_folds) {
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

  train <- remap_predictors(list(train = data_train), predictors)$train

  nrows <- nrow(train)
  cv_predictions_list <- vector("list", cv_folds)

  for(fold in seq_len(cv_folds)) {

    fold_train <- train %>%
      dplyr::filter(cv_fold != fold)

    fold_test <- train %>%
      dplyr::filter(cv_fold == fold)

    weight_vector <- if(is.null(weight)) rep(1, nrow(fold_train)) else fold_train[[weight]]

    glm <- glm(
      formula = formula,
      family = family,
      weights = weight_vector,
      data = fold_train
    )

    cv_predictions_list[[fold]] <- tibble::tibble(
      id = fold_test$id,
      cv_pred_target = predict(glm, newdata = fold_test, type = "response")
    )

  }

  dplyr::bind_rows(!!!cv_predictions_list) %>%
    dplyr::arrange(id) %>%
    dplyr::select(cv_pred_target) %>%
    unlist(use.names = FALSE)

}
