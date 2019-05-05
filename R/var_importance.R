var_importance <- function(setup, direction = c("backward", "forward", "both"), quiet = TRUE) {
  stopifnot(inherits(setup, "setup"))
  direction <- match.arg(direction)

  predictors <- setup$simple_factors
  predictors_collapsed <- paste0(predictors, collapse = " + ")
  formula <- as.formula(paste0(setup$target, " ~ ", predictors_collapsed))
  train <- setup$data_train

  family <- setup$family
  weights <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]

  # prepare simple factors for fitting
  for(var in predictors) {
    x <- train[[var]]

    stopifnot(inherits(x, "simple_factor"))

    levels(train[[var]]) <- attr(x, "model_levels")
  }

  glm <- glm(
    formula = formula,
    family = family,
    weights = weights,
    data = train
  )

  if(quiet) trace <- 0 else trace <- 1

  stepwise_model <- step(object = glm, direction = direction, trace = trace)

  print(formula(stepwise_model))
  broom::tidy(stepwise_model)
}
