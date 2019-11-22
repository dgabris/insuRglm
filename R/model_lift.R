#' Visualize the lift of insuRglm models
#'
#' Visualize the lift chart of one or all saved insuRglm models. The records are first ordered by predictions in ascending order and
#' then divided roughly into several buckets (groups). Average of target variable is computed and displayed for each group
#' separately.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param data Character scalar. Either \code{train} or \code{crossval}. The latter requires running \code{model_crossval} first.
#' @param model Character scalar. Either \code{current} or \code{all}. The latter will display the current and all saved models.
#' @param buckets Integer scalar. Number of groups to divide data into.
#' @param weighted Boolean scalar. Whether the average of target variable in each group should be weighted.
#'
#' @return List of one or more ggplo2 charts.
#' @export
#'
#' @seealso \code{\link{model_save}}, \code{\link{model_crossval}}
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
#'   model_lift(data = 'train', buckets = 10)
#'
#' modeling_cv <- modeling %>%
#'   model_crossval()
#'
#' modeling_cv %>%
#'   model_lift(data = 'crossval', buckets = 5)
#'

model_lift <- function(setup, data = c("train", "crossval"), model = c("current", "all"), buckets = 10, weighted = TRUE) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  data <- match.arg(data)
  model <- match.arg(model)
  stopifnot(is.numeric(buckets) || is.integer(buckets))

  if(inherits(setup, "offset_model")) {
    data <- "train"
    model <- "current"
    message("Offset model has train (formerly test) data only.")
  }

  if(data == "crossval" && is.null(setup$current_model$cv_predictions)) {
    message("No CV predictions found. Please run 'model_crossval()' first.")
    return(setup)
  }

  train <- setup$data_train
  actual <- train[[setup$target]]
  weight_vector <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]

  model_list <- list()

  if(model == "all") {
    for(model_nm in names(setup$ref_models)) {
      model_list[[model_nm]] <- setup$ref_models[[model_nm]]
    }
  }

  model_list$current_model <- setup$current_model

  lapply(names(model_list), function(model_nm) {
    if(data == "train") {
      expected <- model_list[[model_nm]]$train_predictions
    } else {
      expected <- model_list[[model_nm]]$cv_predictions
    }

    lift_buckets(actual, expected, weight_vector, buckets, weighted) %>%
      tidyr::gather(key = type, value = target, actual, expected) %>%
      lift_plot(title = paste0(model_nm, " (", data, ")"))
  })

}
