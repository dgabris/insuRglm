crossval_predict <- function(data_train, model, cv_folds) {
  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(inherits(model, "fitted_model"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  data_attrs <- model$data_attrs
  predictors <- model$predictors
  train <- remap_predictors(data_train, predictors, data_attrs)

  target <- model$target
  target_sym <- rlang::sym(target)
  formula_lhs <- paste0(target, " ~ ")
  formula_rhs <- prepare_formula_rhs(predictors, data_attrs, add_space = FALSE)
  formula <- as.formula(paste0(formula_lhs, formula_rhs))

  family <- model$family
  weight <- model$weight

  cv_predictions_list <- vector("list", cv_folds)

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

    cv_predictions_list[[fold]] <- tibble::tibble(
      id = fold_test$id,
      cv_pred_target = predict(glm, newdata = fold_test, type = "response")
    )

  }

  dplyr::bind_rows(!!!cv_predictions_list) %>%
    dplyr::arrange(id) %>%
    dplyr::pull(cv_pred_target)

}
