variable_importance <- function(setup, direction = c("backward", "forward", "both")) {
  stopifnot(inherits(setup, "setup"))
  direction <- match.arg(direction)
  formula <- as.formula(paste0(setup$target, " ~ ."))
  train <- setup$data_train

  if(setup$family == "tweedie") {
    family <- tweedie(var.power = setup$tweedie_p, link.power = 0)
  } else {
    family <- get(setup$family)
  }

  weights <- setup$weight

  glm <- glm(
    formula = formula,
    family = family,
    weights = weights,
    data = train
  )

  stepwise_model <- step(object = glm, direction = direction)
  coef(stepwise_model)
}
