model_offset <- function(setup) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))

  if(is.null(setup$data_test)) {
    message("Can't do model offseting without a test set.")
    return(setup)
  }

  if(inherits(setup, "offset_model")) {
    message("Model already offset.")
    return(setup)
  }

  current_model <- setup$current_model
  predictors <- current_model$predictors

  for(i in seq_along(predictors)) {
    predictor <- predictors[[i]]
    predictor_sym <- rlang::sym(predictor)

    relativities <- current_model$relativities[[i]]
    mapping <- setNames(relativities$relativity, relativities$orig_level)

    setup <- setup %>%
      factor_modify(
        !!predictor_sym := as_simple_factor(!!predictor_sym),
        !!predictor_sym := offset_term(!!predictor_sym, mapping = !!mapping)
      )
  }

  setup$data_train <- setup$data_test
  setup$data_test <- NULL

  setup$current_model$train_predictions <- setup$current_model$test_predictions
  setup$current_model$test_predictions <- NULL

  class(setup) <- c("offset_model", class(setup))

  setup
}
