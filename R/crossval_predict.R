crossval_predict <- function(data_train, model, cv_folds, seed) {
  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(inherits(model, "fitted_model"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  data_attrs <- model$data_attrs
  target <- model$target
  predictors <- model$predictors

  if(is.null(predictors)) {
    train <- data_train
    formula <- as.formula(paste0(target, " ~ 1"))
  } else {
    train <- remap_predictors(data_train, predictors, data_attrs)

    formula_lhs <- paste0(target, " ~ ")
    formula_rhs <- prepare_formula_rhs(predictors, data_attrs, add_space = FALSE)
    formula <- as.formula(paste0(formula_lhs, formula_rhs))
  }

  family <- model$family
  weight <- model$weight
  offset <- model$offset

  train <- train[c("id", target, weight, offset, predictors, "cv_fold")]
  glm_fun <- model$glm_fun

  cv_predictions_list <- furrr::future_map(seq_len(cv_folds), carrier::crate(function(fold) {
    fold_train <- train %>%
      dplyr::filter(cv_fold != fold)

    fold_test <- train %>%
      dplyr::filter(cv_fold == fold)

    glm <- do.call(
      glm_fun,
      list(
        formula = formula,
        family = family,
        weights = fold_train[[weight]],
        offset = if(!is.null(offset)) log(fold_train[[offset]]) else NULL,
        data = fold_train
      )
    )

    tibble::tibble(
      id = fold_test$id,
      cv_pred_target = stats::predict(glm, newdata = fold_test, type = "response")
    )
  },
  train = train,
  glm_fun = glm_fun,
  formula = formula,
  family = family,
  weight = weight,
  offset = offset,
  `%>%` = `%>%`
  ),
  .options = furrr::future_options(globals = FALSE, seed = seed)
  )

  dplyr::bind_rows(!!!cv_predictions_list) %>%
    dplyr::arrange(id) %>%
    dplyr::pull(cv_pred_target)

}
