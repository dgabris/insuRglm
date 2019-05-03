fit <- function(setup) {
  stopifnot(inherits(setup, "setup"))

  target <- setup$target
  weight <- setup$weight
  predictors <- setup$predictors

  vars <- c(target, weight, predictors)
  train <- setup$data_train[vars]
  weight_vector <- train[[weight]]

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

  betas <- betas(train[predictors], broom::tidy(glm))
  stats <- broom::glance(glm)
  predictions <- predict(glm, newdata = setup$data_train, type = "response")

  setup$current_model <- list(betas, stats, predictions)

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup
}
