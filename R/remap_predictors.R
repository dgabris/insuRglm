remap_predictors <- function(df, predictors, data_attrs) {
  stopifnot(inherits(df, "data.frame"))
  stopifnot(is.character(predictors))

  for(var in predictors) {
    x <- df[[var]]

    var_attrs <- data_attrs[[var]]
    var_class <- var_attrs$class[[1]]
    mapping <- var_attrs$mapping
    base_level <- var_attrs$base_level

    if(var_class == "custom_factor") {
      new_base_level <- as.character(mapping[[which(names(mapping) == base_level)]])

      df[[var]] <- mapping[as.character(x)]
      df[[var]] <- as.factor(df[[var]])
      df[[var]] <- forcats::fct_relevel(df[[var]], new_base_level)

    } else if(var_class == "variate") {
      mapping <- setNames(mapping$actual_level, mapping$orig_level)

      df[[var]] <- mapping[as.character(x)]
      df[[var]] <- as.numeric(df[[var]])

    } else if(var_class == "interaction") {
      df[[var]] <- mapping[as.character(x)]
      df[[var]] <- as.factor(df[[var]])
      df[[var]] <- forcats::fct_relevel(df[[var]], base_level)

    } else if(var_class == "offset") {
      df[[var]] <- mapping[as.character(x)]
      df[[var]] <- as.numeric(df[[var]])

    } else if(var_class == "simple_factor") {
      df[[var]] <- forcats::fct_relevel(x, base_level)
    }
  }

  df

}
