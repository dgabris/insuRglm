model_crossval <- function(setup, cv_folds = 10, stratified = FALSE) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))

  data_train <- setup$data_train %>%
    mutate(id = row_number())

  target <- setup$target
  target_sym <- rlang::sym(target)

  n_losses <- nrow(dplyr::filter(data_train, !!target_sym > 0))
  n_non_losses <- nrow(dplyr::filter(data_train, !!target_sym == 0))
  stopifnot((n_losses + n_non_losses) == nrow(data_train))

  if(stratified && (n_non_losses >= cv_folds)) {

    losses_index <- data_train[[target]] > 0
    non_losses_index <- !losses_index

    cv_fold_losses <- sample(1:cv_folds, n_losses, replace = TRUE)
    cv_fold_non_losses <- sample(1:cv_folds, n_non_losses, replace = TRUE)

    data_train <- train %>% dplyr::mutate(cv_fold = 0)
    data_train$cv_fold[losses_index] <- cv_fold_losses
    data_train$cv_fold[non_losses_index] <- cv_fold_non_losses

  } else {
    data_train <- data_train %>%
      dplyr::mutate(cv_fold = sample(1:cv_folds, nrow(.), replace = TRUE))
  }

  for(model_nm in names(setup$ref_models)) {
    model <- setup$ref_models[[model_nm]]
    setup$ref_models[[model_nm]]$cv_predictions <- crossval_predict(data_train, model, cv_folds)
  }

  current_model <- setup$current_model
  setup$current_model$cv_predictions <- crossval_predict(data_train, current_model, cv_folds)

  setup

}
