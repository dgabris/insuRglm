factor_add <- function(setup, var_symbol) {
  stopifnot(inherits(setup, "setup"))

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
    stopifnot(new_var_char %in% setup$simple_factors)
  }

  predictors <- setup$current_model$predictors

  if(new_var_char %in% predictors) message(paste0("Can't add '", new_var_char, "'. It's already among predictors."))
  setup$current_model$predictors <- unique(c(predictors, new_var_char))

  setup
}
