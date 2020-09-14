prepare_formula_rhs <- function(predictors, data_attrs, add_space) {
  stopifnot(inherits(predictors, "character"))
  stopifnot(inherits(data_attrs, "list"))
  stopifnot(inherits(add_space, "logical"))

  formula_parts <- vector("character", length(predictors))

  for(i in seq_along(predictors)) {
    var <- predictors[[i]]
    var_class <- data_attrs[[var]]$class[[1]]

    new_var <- var
    if(add_space) new_var <- paste0("`", new_var, " `")

    if(var_class == "offset") {
      new_var <- paste0("offset(", new_var, ")")
    }

    if(var_class == "variate") {
      var_degree <- data_attrs[[var]]$degree
      new_var <- paste0("poly(", new_var, ", degree = ", var_degree, ")")
    }

    formula_parts[[i]] <- new_var
  }

  paste0(formula_parts, collapse = " + ")

}
