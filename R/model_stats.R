# TODO - pull request on broom, to add glance.speedglm
# TODO - also change p.value returned from tidy.speedglm from factor to double
model_stats <- function(glm) {
  stopifnot(inherits(glm, "glm") || inherits(glm, "speedglm"))

  if(inherits(glm, "glm")) {
    tibble::tibble(
      broom::glance(glm),
      dispersion = summary(glm)$dispersion
    )
  } else if(inherits(glm, "speedglm")) {
    tibble::tibble(
      null.deviance = glm$nulldev,
      df.null = glm$nulldf,
      logLik = c(glm$logLik, NA_real_)[[1]],
      AIC = c(glm$aic, NA_real_)[[1]],
      BIC = NA_real_,
      deviance = glm$deviance,
      df.residual = glm$df,
      dispersion = glm$dispersion
    )
  }


}
