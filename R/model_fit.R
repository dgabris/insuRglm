model_fit <- function(setup) {
  stopifnot(inherits(setup, "setup"))

  target <- setup$target
  weight <- setup$weight
  predictors <- setup$current_model$predictors

  vars <- c(target, weight, predictors)
  train <- setup$data_train[vars]

  test_exists <- !is.null(setup$data_test)
  test <- if(test_exists) setup$data_test[vars] else NULL

  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  offset_class <- vapply(train, function(x) inherits(x, "offset"), logical(1))

  df_list <- remap_predictors(list(train = train, test = test), predictors)

  train <- df_list$train
  test <- if(test_exists) df_list$test else NULL

  predictors_with_space <- paste0("`", predictors, " `")

  if(any(offset_class)) {
    offset_names <- names(offset_class)[offset_class]
    offset_index <- which(predictors == offset_names)
    predictors_with_space[offset_index] <- paste0("offset(", predictors_with_space[offset_index], ")")
  }

  predictors_collapsed <- paste0(predictors_with_space, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors_collapsed))

  family <- setup$family

  tw <- c(target, weight)
  colnames(train) <- c(tw, paste0(setdiff(colnames(train), tw), " "))
  colnames(test) <- c(tw, paste0(setdiff(colnames(test), tw), " "))

  glm <- glm(
    formula = formula,
    family = family,
    weights = weight_vector,
    data = train
  )

  betas <- betas(predictors, broom::tidy(glm))
  model_stats <- broom::glance(glm)
  train_predictions <- predict(glm, newdata = train, type = "response")
  test_predictions <- if(test_exists) predict(glm, newdata = test, type = "response") else NULL
  factor_tables <- factor_tables(setup, betas, train_predictions)
  relativities <- relativities(factor_tables)

  setup$current_model <- structure(
    list(
      target = target,
      weight = weight,
      family = family,
      predictors = predictors,
      betas = betas,
      model_stats = model_stats,
      factor_tables = factor_tables,
      relativities = relativities,
      train_predictions = train_predictions,
      test_predictions = test_predictions
    ),
    class = "fitted_model"
  )

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup
}
