fit_glm <- function(setup) {
  stopifnot(inherits(setup, "setup"))

  train <- setup$data_train
  target <- setup$target
  predictors <- paste0(setup$predictors, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors))

  if(setup$family == "tweedie") {
    family <- tweedie(var.power = setup$tweedie_p, link.power = 0)
  } else {
    family <- get(setup$family)
  }

  weights <- setup$weight

  # TODO - adjust custom factor and variate before the glm fitting

  glm <- glm(
    formula = formula,
    family = family,
    weights = weights,
    data = train
  )

  betas <- betas(train, glm)
  stats <- glance(glm)
  predictions <- predict(glm, newdata = setup$data_train, type = "response")

  setup$current_model <- list(betas, stats, predictions)

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup
}
