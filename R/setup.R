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
#' @param glm_backend Character scalar. Either 'speedglm' or 'stats'. Choosing 'speedglm' results in using
#' \code{speedglm::speedglm} as glm backend, while choosing 'stats' will result in traditional \code{stats::glm}.
#' @param folder Character scalar. Path to an existing folder where setup/model files will be stored.
#' @param load_file_nm Character scalar. Filename of an existing setup object created by running setup.
#' Must be within folder specified by \code{folder}. Can be without the '_setup.rds' suffix.
#' @param save_file_nm Character scalar. Filename of a setup object saved during this run of the setup function.
#' Will be saved within the folder specified by \code{folder},
#' @param seed Numeric scalar. Seed for reproducible random number generation, e.g. for creating CV folds.
#'
#' @return List of class \code{setup}. Contains attributes and objects used by other functions in the package.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' require(dplyr) # for the pipe operator#'
#'
#' # poisson distribution target
#' data('freq_train')
#'
#' setup <- setup(
#'   data_train = freq_train,
#'   target = 'freq',
#'   offset = 'exposure',
#'   family = 'poisson',
#'   keep_cols = c('pol_nbr', 'premium')
#' )
#'
#' # gamma distribution target
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
#' # tweedie distribution - burning cost
#' data('bc_train')
#'
#' setup <- setup(
#'   data_train = bc_train,
#'   target = 'bc',
#'   weight = 'exposure',
#'   family = 'tweedie',
#'   tweedie_p = 1.75, # use tweedie::tweedie.profile to determine the best value
#'   keep_cols = c('pol_nbr', 'premium')
#' )
#'
#' # tweedie distribution - loss ratio
#' data('lr_train')
#'
#' setup <- setup(
#'   data_train = lr_train,
#'   target = 'lr',
#'   weight = 'premium',
#'   family = 'tweedie',
#'   tweedie_p = 1.75, # use tweedie::tweedie.profile to determine the best value
#'   keep_cols = c('pol_nbr', 'exposure')
#' )
#'

setup <- function(data_train, data_test = NULL, target, weight = NULL, offset = NULL, family = c("poisson", "gamma", "tweedie"),
                  tweedie_p = NULL, simple_factors = NULL, keep_cols = NULL, glm_backend = c("speedglm", "stats"),
                  folder = getwd(), load_file_nm = NULL, save_file_nm = NULL,  seed = NULL) {

  if(!(length(folder) == 1 && is.character(folder))) stop("'folder' must be character scalar.")
  if(!file.exists(folder)) stop(paste0("folder '", folder, "' doesn't exist."))

  if(!is.null(load_file_nm)) {
    if(!(length(load_file_nm) == 1 && is.character(load_file_nm))) stop("'load_file_nm' must be character scalar.")

    setup_files <- c(
      load_file_nm,
      paste0(load_file_nm, ".rds"),
      paste0(load_file_nm, "_setup"),
      paste0(load_file_nm, "_setup.rds")
    ) %>%
    vapply(function(x) file.path(folder, x), character(1))

    setup_files <- setup_files[vapply(setup_files, file.exists, logical(1))]

    if(length(setup_files) > 0) {
      setup_file <- setup_files[[1]]
    } else {
      stop(paste0("file '", file.path(folder, load_file_nm),  "' doesn't exist."))
    }

    potential_setup <- readr::read_rds(setup_file)

    if(inherits(potential_setup, "setup")) {
      return(potential_setup)
    }
  }

  if(!is.null(save_file_nm)) {
    if(!(length(save_file_nm) == 1 && is.character(save_file_nm))) stop("'save_file_nm' must be character scalar.")
  }

  if(!is.null(seed)) {
    if(!(length(seed) == 1 && is.numeric(seed))) stop("'seed' must be numeric scalar.")
  }

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
    cat("Checking train/test schema consistency\n")
    schema_train <- vapply(data_train, class, character(1))
    schema_test <- vapply(data_test, class, character(1))

    schema_test <- schema_test[match(names(schema_train), names(schema_test))]

    consistency_error <- dplyr::coalesce(schema_train != schema_test, TRUE)

    if(any(consistency_error)) {
      stop(
        paste0(
          "'data_train' and 'data_test' don't have the same columns or their types. ",
          "Please check this using 'colnames' and 'class' functions."
          )
        )
    }
  }

  if(!(inherits(target, 'character') && length(target) == 1)) stop("'target' must be a character scalar")
  if(!target %in% colnames(data_train)) stop('Target variable not in the dataset')

  if(!is.null(weight)) {
    if(!inherits(weight, 'character')) stop("'weight' must be a character scalar")
    if(!weight %in% colnames(data_train)) stop('Weight variable not in the dataset')
  } else {
    cat("Creating weight column. Each record will have the same weight.\n")
    weight <- '_weight'

    data_train[[weight]] <- 1

    if(!is.null(data_test)) {
      data_test[[weight]] <- 1
    }
  }

  if(!is.null(offset)) {
    if(!(inherits(offset, 'character') && length(offset) == 1)) stop("'offset' must be a character scalar")
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

  levels_length <- vapply(data_train[simple_factors], function(x) length(unique(x)), integer(1))

  if(any(levels_length > 255)) {
    stop(
      paste0(
        "Some of the 'simple_factors' columns have more than 255 unique values: ",
        paste0(simple_factors[levels_length > 255], collapse = ", "),
        ". If these are not predictors, but columns you want to keep, please include them in 'keep_cols' argument"
      )
    )
  }

  if(any(vapply(data_train[simple_factors], class, character(1)) != 'factor')) {
    cat("Converting all predictor columns to `factor` class.\n")

    data_train[simple_factors] <- lapply(data_train[simple_factors], function(x) {
      if(!inherits(x, 'factor')) as.factor(x) else x
    })

    if(!is.null(data_test)) {
      data_test[simple_factors] <- lapply(data_test[simple_factors], function(x) {
        if(!inherits(x, 'factor')) as.factor(x) else x
      })
    }
  }

  stopifnot(all(vapply(data_train[simple_factors], class, character(1)) == "factor"))

  family <- match.arg(family)

  if(family %in% c('poisson', 'gamma') && !is.null(tweedie_p)) {
    message("family is 'poisson' or 'gamma', 'tweedie_p' will be ignored")
  }

  if(family %in% c('tweedie')) {
    if(is.null(tweedie_p)) stop("Please provide 'tweedie_p'")
    if(!(is.numeric(tweedie_p) && length(tweedie_p) == 1)) stop("'tweedie_p' must be a numeric scalar")
    if(tweedie_p <= 1 || tweedie_p >= 2) stop("'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.")
  }

  tweedie_p <- if(is.null(tweedie_p)) 0 else tweedie_p

  family_nm <- family

  family <- switch(
    family,
    poisson = poisson(link = "log"),
    gamma = Gamma(link = "log"),
    tweedie = statmod::tweedie(var.power = tweedie_p, link.power = 0)
  )

  if(!is.null(data_test)) {
    for(var in simple_factors) {
      train_levels <- levels(data_train[[var]])
      test_levels <- levels(data_test[[var]])

      not_in_train <- test_levels[!(test_levels %in% train_levels)]

      if(length(not_in_train) > 0) {
        stop(paste0(var, " has these values in test set, that are not present in train: ", paste0(not_in_train, collapse = ", ")))
      }

      data_test[[var]] <- factor(data_test[[var]], levels = train_levels)
    }
  }

  train_list <- as.list(data_train[simple_factors])

  if(!is.null(data_test)) {
    test_list <- as.list(data_test[simple_factors])
  } else {
    test_list <- vector("list", length(simple_factors))
    names(test_list) <- simple_factors
  }

  weight_vector <- data_train[[weight]]
  target_vector <- data_train[[target]]

  # it seems like the parallel processing version is slower than sequential?
  # keeping purrr instead of furrr for now
  cat("Setting levels with the highest weight as base levels.\n")

  simple_factor_list <- purrr::pmap(
    list(train_list, test_list, simple_factors),
    carrier::crate(function(x_train, x_test, var_nm) {

      result <- list()

      orig_levels <- levels(x_train)
      levels_by_weight <- names(sort(tapply(weight_vector, x_train, sum), decreasing = TRUE))
      base_level <- levels_by_weight[[1]]

      result$x_train <- simple_factor(x_train, orig_levels, base_level, var_nm)

      if(!is.null(x_test)) {
        result$x_test <- simple_factor(x_test, orig_levels, base_level, var_nm)
      } else {
        result$x_test <- list(NULL)
      }

      result$x_attrs <- attributes(result$x_train)

      result$obs_avg_df <- compute_obs_avg(result$x_train, target_vector, weight_vector)

      result
    },
    simple_factor = simple_factor,
    compute_obs_avg = compute_obs_avg,
    target_vector = target_vector,
    weight_vector = weight_vector
    )
    # .options = furrr::future_options(globals = FALSE)
  ) %>%
  setNames(vapply(., function(x) x$x_attrs$var_nm, character(1)))

  data_train <- tibble::tibble(
    data_train[c(target, weight, offset, keep_cols)],
    tibble::as_tibble(lapply(simple_factor_list, function(x) x$x_train))
  )

  if(!is.null(data_test)) {
    data_test <- tibble::tibble(
      data_test[c(target, weight, offset, keep_cols)],
      tibble::as_tibble(lapply(simple_factor_list, function(x) x$x_test))
    )
  }

  data_attrs <- lapply(simple_factor_list, function(x) x$x_attrs)

  obs_avg_tables <- lapply(simple_factor_list, function(x) x$obs_avg_df)

  glm_backend <- match.arg(glm_backend)
  glm_fun <- switch(glm_backend, stats = stats::glm, speedglm = speedglm::speedglm)

  current_model <- structure(
    list(
      target = target,
      weight = weight,
      offset = offset,
      family = family,
      predictors = NULL,
      data_attrs = data_attrs,
      glm_fun = glm_fun
    ),
    class = "unfitted_model"
  )

  if(is.null(seed)) {
    seed <- sample(1:999999, size = 1)
  } else {
    seed <- as.integer(abs(seed))
  }

  if(is.null(save_file_nm)) {
    base_nm <- paste0(target, "_", family_nm)
  } else {
    base_nm <- stringr::str_replace_all(save_file_nm, c("_setup" = "", "\\.rds" = ""))
  }

  save_file_nm <- paste0(base_nm, "_setup.rds")

  setup <- structure(
    list(
      target = target,
      weight = weight,
      offset = offset,
      family = family,
      simple_factors = simple_factors,
      glm_fun = glm_fun,
      folder = folder,
      base_nm = base_nm,
      seed = seed,
      obs_avg_tables = obs_avg_tables,
      data_train = data_train,
      data_test = if(!is.null(data_test)) data_test else NULL,
      current_model = current_model
    ),
    class = "setup"
  )

  # fit and save intercept only model
  setup <- setup %>%
    model_fit() %>%
    model_save("intercept_model")

  cat(paste0("Saving setup object to '", folder, "'"))
  readr::write_rds(setup, file.path(folder, save_file_nm), compress = "gz")

  cat("\n")
  cat("Setup - OK\n")
  cat("\n")
  basic_summary(setup)

  setup
}

#' @export
print.setup <- function(x, ...) {
  cat(paste0("Target: ", x$target))
  cat("\n")
  cat(paste0("Weight: ", x$weight))
  cat("\n")
  cat(paste0("Actual Predictors: ", paste0(x$current_model$predictors, collapse = ", ")))
  cat("\n")
  cat(paste0("Available Factors: ",
               paste0(setdiff(x$simple_factors, x$current_model$predictors), collapse = ", "))
  )
  cat("\n")
}
