#' Fit the insuRglm model
#'
#' Fits the model with the current model formula. Computes and saves back many new attributes and objects.
#' This is a required step before using \code{model_visualize}, \code{model_compare}, \code{model_save},
#' \code{model_betas}, \code{model_crossval}, \code{model_lift}, \code{model_export} and others.
#' In case of a big dataset (especially many columns), declaring \code{future::plan(multiprocess)} beforehand
#' might help to speed up the process.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param glm_backend Character scalar. Either 'setup', 'speedglm' or 'stats'. Choosing 'setup', which is a default
#' choice results in using the \code{glm_backend} set during setup. Choosing 'speedglm' or 'stats' will temporarily
#' override this by using \code{speedglm::speedglm} or \code{stats::glm}.
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = sev_train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' # parallel processing is supported and may be faster on bigger datasets
#' plan(multiprocess)
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

model_fit <- function(setup, glm_backend = c("setup", "speedglm", "stats")) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')

  glm_backend <- match.arg(glm_backend)
  glm_fun <- switch(glm_backend, setup = setup$glm_fun, stats = stats::glm, speedglm = speedglm::speedglm)

  if(inherits(setup, "offset_model")) {
    message("Note: Using test data and fitting intercept only.")
  }

  data_attrs <- lapply(setup$data_train[setup$simple_factors], attributes)

  target <- setup$target
  weight <- setup$weight
  offset <- setup$offset
  predictors <- setup$current_model$predictors

  vars <- c(target, weight, offset, predictors)
  train <- setup$data_train[vars]

  # test_exists <- !is.null(setup$data_test)
  # test <- if(test_exists) setup$data_test[vars] else NULL

  if(is.null(predictors)) {
    formula <- as.formula(paste0(target, " ~ 1"))

  } else {
    offset_class <- vapply(train, function(x) inherits(x, "offset"), logical(1))
    variate_class <- vapply(train, function(x) inherits(x, "variate"), logical(1))
    variate_degrees <- vapply(train[variate_class], function(x) attr(x, "degree"), numeric(1))

    train <- remap_predictors(train, predictors, data_attrs)
    # TODO
    # think how test set should be treated

    formula_lhs <- paste0(target, " ~ ")
    formula_rhs <- prepare_formula_rhs(predictors, data_attrs, add_space = TRUE)
    formula <- as.formula(paste0(formula_lhs, formula_rhs))

    tw <- c(target, weight)
    colnames(train) <- c(tw, paste0(setdiff(colnames(train), tw), " "))
    # if(test_exists) colnames(test) <- c(tw, paste0(setdiff(colnames(test), tw), " "))
  }

  family <- setup$family

  glm <- do.call(
    glm_fun,
    list(
      formula = formula,
      family = family,
      weights = train[[weight]],
      offset = if(!is.null(offset)) log(train[[offset]]) else NULL,
      data = train
    )
  )

  betas <- betas(predictors, broom::tidy(glm))
  beta_triangles <- beta_triangles(betas, glm, data_attrs[predictors])
  model_stats <- model_stats(glm)
  train_predictions <- unname(predict(glm, newdata = train, type = "response"))
  # test_predictions <- if(test_exists) predict(glm, newdata = test, type = "response") else NULL
  test_predictions <- NULL
  current_baseline <- adjust_baseline(betas, data_attrs[predictors])
  factor_tables <- factor_tables(setup, betas, current_baseline, train_predictions)
  relativities <- relativities(factor_tables, current_baseline)
  # leverage_plots <- leverage_plots(glm)

  setup$current_model <- structure(
    list(
      target = target,
      weight = weight,
      offset = offset,
      family = family,
      predictors = predictors,
      data_attrs = data_attrs,
      glm_fun = glm_fun,
      betas = betas,
      beta_triangles = beta_triangles,
      model_stats = model_stats,
      current_baseline = current_baseline,
      factor_tables = factor_tables,
      relativities = relativities,
      train_predictions = train_predictions,
      test_predictions = test_predictions,
      cv_predictions = NULL
      # leverage_plots = leverage_plots
    ),
    class = "fitted_model"
  )

  if(!inherits(setup, "modeling")) {
    class(setup) <- c("modeling", class(setup))
  }

  setup_copy <- setup
  setup_copy$data_train <- NULL
  setup_copy$data_test <- NULL
  readr::write_rds(setup_copy, file.path(setup_copy$folder, paste0(setup_copy$base_nm, "_model.rds")))

  setup
}
