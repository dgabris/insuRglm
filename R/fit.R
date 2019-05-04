fit <- function(setup) {
  stopifnot(inherits(setup, "setup"))

  target <- setup$target
  weight <- setup$weight
  predictors <- setup$predictors

  vars <- c(target, weight, predictors)
  train <- setup$data_train[vars]
  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  predictors_collapsed <- paste0(predictors, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors_collapsed))

  if(setup$family == "tweedie") {
    family <- tweedie::tweedie(var.power = setup$tweedie_p, link.power = 0)
  } else {
    family <- get(setup$family)
  }

  # prepare simple factors, custom factors and variates for modeling
  for(var in predictors) {
    x <- train[[var]]

    if(inherits(x, "custom_factor")) {
      mapping <- attr(x, "mapping")
      train[[var]] <- mapping[as.character(x)]
      train[[var]] <- as.factor(train[[var]])

    } else if(inherits(x, "variate")) {
      mapping <- attr(x, "mapping")
      train[[var]] <- mapping[as.character(x)]
      train[[var]] <- as.numeric(train[[var]])

    } else if(inherits(x, "simple_factor")) {
      model_levels <- attr(x, "model_levels")
      levels(train[[var]]) <- model_levels
    }
  }

  glm <- glm(
    formula = formula,
    family = family,
    weights = weight_vector,
    data = train
  )

  betas <- betas(predictors, broom::tidy(glm))
  model_stats <- broom::glance(glm)
  predictions <- predict(glm, newdata = train, type = "response")
  factor_tables <- factor_tables(setup, betas, predictions)
  relativities <- relativities(factor_tables)

  setup$current_model <- list(
    betas = betas,
    model_stats = model_stats,
    factor_tables = factor_tables,
    relativities = relativities
  )

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup
}
