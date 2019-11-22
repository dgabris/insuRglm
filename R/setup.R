#' Setup your modeling workflow
#'
#' Creates a setup object that is the basis for any insuRglm modeling workflow.
#' This object is subsequently used as a main input in most functions in the package.
#'
#' @note Short summary of the train/test datasets is written to the console
#'
#' @param data_train Dataframe. Training data
#' @param data_test Dataframe. Test data
#' @param target Character scalar. Name of the target variable
#' @param weight Character scalar. Name of the weight variable
#' @param offset. Character scalar. Name of the offset variable, applicable for \code{poisson} family
#' @param family Character scalar. Name of distribution family. One of \code{poisson}, \code{tweedie} or \code{gamma}
#' @param tweedie_p Numeric scalar. Tweedie variance power, if family \code{tweedie} is used
#' @param simple_factors Character vector. Names of potential predictors. These predictors need to be \code{factor} class.
#' @param keep_cols Character vector. Names of columns that are not potential predictors, but should be kept in data.
#' @param seed Numeric scalar. Seed for random number generation. Currently doesn't have any effect.
#'
#' @return List of class \code{setup}. Contains attributes and objects used by other functions in the package.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#' data('sev_test')
#'
#' head(train)
#'
#' # minimal
#' setup <- setup(
#'   data_train = train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' # adding test set, specifying potential predictors manually
#' setup <- setup(
#'   data_train = train,
#'   data_test = test,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   simple_factors = c('agecat', 'veh_age', 'veh_body'),
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#'

setup <- function(data_train, data_test = NULL, target, weight = NULL, offset = NULL, family = c("poisson", "gamma", "tweedie"),
                  tweedie_p = NULL, simple_factors = NULL, keep_cols = NULL, seed = NULL) {

  dfs <- list(data_train, data_test) %>%
    purrr::keep(function(x) !is.null(x))

  if(!(any(vapply(dfs, function(x) inherits(x, 'data.frame'), logical(1))))) {
    if(!is.null(data_test)) {
      stop("'data_train' and 'data_test' must be of class 'data.frame'")
    } else {
      stop("'data_train' must be of class 'data.frame'")
    }
  }

  if(!is.null(data_test)) {
    schema_train <- vapply(data_train, class, character(1))
    schema_test <- vapply(data_test, class, character(1))

    schema_test <- schema_test[match(names(schema_train), names(schema_test))]

    if(any(schema_train != schema_test)) {
      stop(
        paste0(
          "'data_train' and 'data_test' don't have the same columns or their types. ",
          "Please check this using 'colnames' and 'class' functions."
          )
        )
    }
  }

  if(!inherits(target, 'character')) stop("'target' must be a character scalar")
  if(!target %in% colnames(data_train)) stop('Target variable not in the dataset')

  if(!is.null(weight)) {
    if(!inherits(weight, 'character')) stop("'weight' must be a character scalar")
    if(!weight %in% colnames(data_train)) stop('Weight variable not in the dataset')
  } else {
    weight <- '_weight'
    lapply(dfs, function(x) x[weight] <- 1)
    message("'weight' was not provided, each record will have the same weight")
  }

  if(!is.null(offset)) {
    if(!inherits(offset, 'character')) stop("'offset' must be a character scalar")
    if(!offset %in% colnames(data_train)) stop('Offset variable not in the dataset')
  }

  if(!is.null(keep_cols)) {
    if(!inherits(keep_cols, 'character')) stop("'keep_cols' must be a character vector")
    if(!all(keep_cols %in% colnames(data_train))) stop("Some of the 'keep_cols' are not in the dataset")
  }

  if(is.null(simple_factors)) {
    simple_factors <- setdiff(colnames(data_train), c(target, weight, offset, keep_cols))
  } else {
    if(!inherits(simple_factors, 'character')) {
      stop("'simple_factors' must be a character vector of names of potential predictor columns in the dataset")
    } else {
      simple_factors <- unique(simple_factors)
    }
  }

  if(!all(simple_factors %in% colnames(data_train))) {
    stop("Some of the 'simple_factors' are not in the dataset")
  }

  if(any(vapply(data_train[simple_factors], class, character(1)) != 'factor')) {
    levels_length <- vapply(data_train[simple_factors], function(x) length(unique(x)), integer(1))

    if(any(levels_length > 255)) {
      stop(
        paste0(
          "Tried coercing the predictors to 'factor' class, but some of them have more than 255 unique values: ",
          paste0(simple_factors[levels_length > 255], collapse = ', '),
          ". If these are not predictors, but columns you want to keep, please include them in 'keep_cols' argument"
        )
      )
    } else {
      data_train[simple_factors] <- lapply(data_train[simple_factors], function(x) {
        if(!inherits(x, 'factor')) as.factor(x) else x
      })

      if(!is.null(data_test)) {
        data_test[simple_factors] <- lapply(data_test[simple_factors], function(x) {
          if(!inherits(x, 'factor')) as.factor(x) else x
        })
      }

      message("All the predictors are now coerced to 'factor' class")
    }
  }

  stopifnot(all(vapply(data_train[simple_factors], class, character(1)) == "factor"))

  family <- match.arg(family)

  if(family %in% c('poisson') && is.null(offset)) {
    message("No 'offset' provided for family 'poisson', will treat 'weight' as 'offset'")
    offset <- weight
  }

  if(family %in% c('poisson', 'gamma') && !is.null(tweedie_p)) {
    message("family is 'poisson' or 'gamma', 'tweedie_p' will be ignored")
  }

  if(family %in% c('tweedie') && (is.null(tweedie_p) || (tweedie_p <= 1 || tweedie_p >= 2))) {
    stop("'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.")
  }

  tweedie_p <- if(is.null(tweedie_p)) 0 else tweedie_p

  family <- switch(
    family,
    poisson = poisson(link = "log"),
    gamma = Gamma(link = "log"),
    tweedie = statmod::tweedie(var.power = tweedie_p, link.power = 0)
  )

  all_data <- dplyr::bind_rows(data_train, data_test)

  for(var in simple_factors) {

    orig_levels <- levels(all_data[[var]])
    levels_by_weight <- names(sort(tapply(all_data[[weight]], all_data[[var]], sum), decreasing = TRUE))
    base_level <- levels_by_weight[[1]]

    data_train[[var]] <- simple_factor(data_train[[var]], orig_levels, base_level)

    if(!is.null(data_test)) {
      data_test[[var]] <- simple_factor(data_test[[var]], orig_levels, base_level)
    }
  }

  current_model <- structure(
    list(
      target = target,
      weight = weight,
      offset = offset,
      family = family,
      predictors = NULL
    ),
    class = "unfitted_model"
  )

  setup <- structure(
    list(
      target = target,
      weight = weight,
      offset = offset,
      family = family,
      simple_factors = simple_factors,
      seed = seed,
      data_train = tibble::as_tibble(data_train),
      data_test = if(!is.null(data_test)) tibble::as_tibble(data_test) else NULL,
      current_model = current_model
    ),
    class = "setup"
  )

  print("Setup - OK")
  print("")
  basic_summary(setup)

  setup
}
