#' Add predictor to the current model formula
#'
#' Adds the predictor to the list of predictors. These will be used in model formula when \code{model_fit} is called.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param var_symbol Unquoted symbol. Predictor to be added. Must be present in the modeling dataset.
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
#' print(setup)
#'
#' # not fitted yet
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat)
#'
#' print(modeling)
#'
#'

factor_add <- function(setup, var_symbol) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')

  new_var_expr <- rlang::enexpr(var_symbol)
  new_var_char <- as.character(new_var_expr)

  is_interaction <- inherits(setup$data_train[[new_var_char]], "interaction")
  is_component <- inherits(setup$data_train[[new_var_char]], "component")

  if(is_component) {
    stop(paste0("Adding single component of an interaction to the model is not allowed. Please add the whole interaction '",
         attr(setup$data_train[[new_var_char]], "parent_var"), "' instead."))

  } else if(is_interaction) {
    # do nothing

  } else {
    if(!new_var_char %in% setup$simple_factors) {
      if(new_var_char %in% colnames(setup$data_train)) {
        stop(paste0("'", new_var_char, "' is not a predictor."))
      } else {
        stop(paste0("'", new_var_char, "' is not in the modeling dataset."))
      }
    }
  }

  predictors <- setup$current_model$predictors

  if(new_var_char %in% predictors) {
    message(paste0("Can't add '", new_var_char, "'. It's already among predictors."))

  } else {
    setup$current_model$predictors <- unique(c(predictors, new_var_char))

    class(setup$current_model) <- "unfitted_model"
  }

  setup

  # x <- setup$data_train[[new_var_char]]
  # is_interaction <- inherits(x, "interaction")
  #
  # if(is_interaction) {
  #   main_effects <- attr(x, "main_effects")
  #   to_add <- main_effects[!main_effects %in% predictors]
  #
  #   if(length(to_add) > 0) {
  #     message(
  #       paste0("Make sure to add main effect(s) ", paste0(paste0("'", to_add, "'"), collapse = ", "),
  #              " to the model formula as well.")
  #     )
  #   }
  #
  #   components <- attr(x, "components")
  #
  #   if(length(main_effects) == 3) {
  #     to_add <- components[!components %in% predictors]
  #
  #     if(length(to_add) > 0) {
  #       message(
  #         paste0("Make sure to add interaction(s) ", paste0(paste0("'", to_add, "'"), collapse = ", "),
  #                " to the model formula as well.")
  #       )
  #     }
  #   }
  # }

  # is_interaction <- length(new_var_char) >= 3 && new_var_char[[1]] == "*"
  #
  # if(is_interaction) {
  #   interaction_vars <- c(
  #     trimws(unlist(stringr::str_split(new_var_char[[2]], "\\*"))),
  #     new_var_char[[3]]
  #   )
  #
  #   interaction_syms <- rlang::syms(interaction_vars)
  #
  #   stopifnot(all(vapply(interaction_vars, function(x) x %in% setup$simple_factors, logical(1))))
  #   stopifnot(all(vapply(interaction_vars, function(x) x %in% setup$current_model$predictors, logical(1))))
  #
  #   new_var_sym <- rlang::sym(paste(interaction_vars, collapse = "*"))
  #
  #   setup <- setup %>%
  #     factor_modify(!!new_var_sym := my_interaction(list(!!!interaction_syms)))
  #
  #   new_var_char <- as.character(new_var_sym)
  #
  # } else {
  #   if(!new_var_char %in% setup$simple_factors) stop('This predictor is not present in the dataset provided to setup object.')
  # }
}
