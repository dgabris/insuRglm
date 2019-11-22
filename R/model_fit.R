#' Fit the insuRglm model
#'
#' Fits the model with the current model formula. Computes and saves back many new attributes and objects.
#' This is a required step before using \code{model_visualize}, \code{model_compare}, \code{model_save},
#' \code{model_betas}, \code{model_crossval}, \code{model_lift}, \code{model_export} and others
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat) %>%
#'   model_fit()
#'
#' modeling %>%
#'   model_visualize(factors = 'fitted')
#'
#' modeling %>%
#'   model_visualize(factors = 'unfitted')
#'

model_fit <- function(setup) {
  stopifnot(inherits(setup, "setup"))

  if(inherits(setup, "offset_model")) {
    message("Note: Using test data and fitting intercept only.")
  }

  target <- setup$target
  weight <- setup$weight
  predictors <- setup$current_model$predictors

  vars <- c(target, weight, predictors)
  train <- setup$data_train[vars]

  test_exists <- !is.null(setup$data_test)
  test <- if(test_exists) setup$data_test[vars] else NULL

  weight_vector <- if(is.null(weight)) rep(1, nrow(train)) else train[[weight]]

  offset_class <- vapply(train, function(x) inherits(x, "offset"), logical(1))
  variate_class <- vapply(train, function(x) inherits(x, "variate"), logical(1))
  variate_degrees <- vapply(train[variate_class], function(x) attr(x, "degree"), numeric(1))

  df_list <- list(train = train)
  df_list$test <- test

  df_list <- remap_predictors(df_list, predictors)

  train <- df_list$train
  test <- if(test_exists) df_list$test else NULL

  predictors_with_space <- paste0("`", predictors, " `")

  if(any(offset_class)) {
    offset_names <- names(offset_class)[offset_class]
    offset_index <- match(offset_names, predictors)
    predictors_with_space[offset_index] <- paste0("offset(", predictors_with_space[offset_index], ")")
  }

  if(any(variate_class)) {
    variate_names <- names(variate_class)[variate_class]
    variate_index <- match(variate_names, predictors)
    predictors_with_space[variate_index] <- paste0(
      "poly(", predictors_with_space[variate_index], ", degree = ", variate_degrees, ")"
    )
  }

  predictors_collapsed <- paste0(predictors_with_space, collapse = " + ")
  formula <- as.formula(paste0(target, " ~ ", predictors_collapsed))

  family <- setup$family

  tw <- c(target, weight)
  colnames(train) <- c(tw, paste0(setdiff(colnames(train), tw), " "))
  if(test_exists) colnames(test) <- c(tw, paste0(setdiff(colnames(test), tw), " "))

  glm <- glm(
    formula = formula,
    family = family,
    weights = weight_vector,
    data = train
  )

  betas <- betas(predictors, broom::tidy(glm))
  beta_triangles <- beta_triangles(betas, glm)
  model_stats <- dplyr::bind_cols(broom::glance(glm), dispersion = summary(glm)$dispersion)
  train_predictions <- predict(glm, newdata = train, type = "response")
  test_predictions <- if(test_exists) predict(glm, newdata = test, type = "response") else NULL
  factor_tables <- factor_tables(setup, betas, train_predictions)
  relativities <- relativities(factor_tables, betas)
  leverage_plots <- leverage_plots(glm)

  setup$current_model <- structure(
    list(
      target = target,
      weight = weight,
      family = family,
      predictors = predictors,
      betas = betas,
      beta_triangles = beta_triangles,
      model_stats = model_stats,
      factor_tables = factor_tables,
      relativities = relativities,
      train_predictions = train_predictions,
      test_predictions = test_predictions,
      cv_predictions = NULL,
      leverage_plots = leverage_plots
    ),
    class = "fitted_model"
  )

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup
}
