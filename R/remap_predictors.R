remap_predictors <- function(df_list, predictors) {
  stopifnot(typeof(df_list) == "list")
  stopifnot(all(vapply(df_list, function(x) inherits(x, "data.frame"), logical(1))))
  stopifnot(is.character(predictors))

  lapply(df_list, function(df) {

    for(var in predictors) {
      x <- df[[var]]

      if(inherits(x, "custom_factor")) {

        mapping <- attr(x, "mapping")
        base_level <- attr(x, "base_level")
        new_base_level <- as.character(mapping[[which(names(mapping) == base_level)]])

        df[[var]] <- mapping[as.character(x)]
        df[[var]] <- as.factor(df[[var]])
        df[[var]] <- forcats::fct_relevel(df[[var]], new_base_level)

      } else if(inherits(x, "variate")) {
        mapping_df <- attr(x, "mapping")
        mapping <- setNames(mapping_df$actual_level, mapping_df$orig_level)

        df[[var]] <- mapping[as.character(x)]
        df[[var]] <- as.numeric(df[[var]])

      } else if(inherits(x, "interaction")) {
        mapping <- attr(x, "mapping")
        base_level <- attr(x, "base_level")

        df[[var]] <- mapping[as.character(x)]
        df[[var]] <- as.factor(df[[var]])
        df[[var]] <- forcats::fct_relevel(df[[var]], base_level)

      } else if(inherits(x, "offset")) {
        mapping <- attr(x, "mapping")

        df[[var]] <- mapping[as.character(x)]
        df[[var]] <- as.numeric(df[[var]])

      } else if(inherits(x, "simple_factor")) {
        base_level <- attr(x, "base_level")
        df[[var]] <- forcats::fct_relevel(x, base_level)
      }
    }

    df

  })

}
