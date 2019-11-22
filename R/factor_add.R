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
#'   data_train = train,
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

  is_interaction <- length(new_var_char) >= 3 && new_var_char[[1]] == "*"

  if(is_interaction) {
    interaction_vars <- c(
      trimws(unlist(stringr::str_split(new_var_char[[2]], "\\*"))),
      new_var_char[[3]]
    )

    interaction_syms <- rlang::syms(interaction_vars)

    stopifnot(all(vapply(interaction_vars, function(x) x %in% setup$simple_factors, logical(1))))
    stopifnot(all(vapply(interaction_vars, function(x) x %in% setup$current_model$predictors, logical(1))))

    new_var_sym <- rlang::sym(paste(interaction_vars, collapse = "*"))

    setup <- setup %>%
      factor_modify(!!new_var_sym := my_interaction(list(!!!interaction_syms)))

    new_var_char <- as.character(new_var_sym)

  } else {
    if(!new_var_char %in% setup$simple_factors) stop('This predictor is not present in the dataset provided to setup object.')
  }

  predictors <- setup$current_model$predictors

  if(new_var_char %in% predictors) message(paste0("Can't add '", new_var_char, "'. It's already among predictors."))
  setup$current_model$predictors <- unique(c(predictors, new_var_char))

  setup
}
