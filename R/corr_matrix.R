corr_matrix <- function(setup, type = c("graph", "table")) {
  stopifnot(inherits(setup, "setup"))
  type <- match.arg(type)

  if(inherits(setup, "modeling")) {
    simple_factors <- setup$model_factors
  } else {
    simple_factors <- setup$simple_factors
  }

  train <- setup$data_train[simple_factors]

  combinations_df <-
    tidyr::crossing(var1 = simple_factors, var2 = simple_factors) %>%
    distinct()

  cramers_v_vector <- vector(length = nrow(combinations_df))

  for(i in 1:nrow(combinations_df)) {
    first_col <- combinations_df[[i, 1]]
    second_col <- combinations_df[[i, 2]]

    cramers_v_vector[[i]] <- compute_cramers_v(data[[first_col]], data[[second_col]])
  }

  assoc_df <- cbind(combinations_df, cramers_v = cramers_v_vector)

  deduped_assoc_df <- assoc_df %>%
    mutate(var_both = pmap(list(var1, var2), function(x, y) paste0(sort(c(x, y)), collapse = " "))) %>%
    tidyr::unnest() %>%
    distinct(var_both, cramers_v) %>%
    tidyr::separate(var_both, into = c("var1", "var2"), sep = " ") %>%
    select(var1, var2, cramers_v)

  if(type == "table") {
    deduped_assoc_df
  } else {
    deduped_assoc_df %>%
      mutate(cramers_v = if_else(var1 == var2, NA_real_, cramers_v)) %>%
      mutate(var1 = as.factor(var1)) %>%
      mutate(var2 = factor(var2, levels = rev(levels(var1)))) %>%
      ggplot(aes(x = var1, y = var2, color = cramers_v)) +
      geom_point(size = 5) +
      ggtitle("Association Matrix") +
      labs(x = NULL, y = NULL, color = "Cramer's V") +
      scale_color_gradientn(colours = c("white", "orange", "red"), values = c(0, 0.3, 1)) +
      theme(plot.title = element_text(hjust = 0.45),
            axis.text.x = element_text(angle = 30, hjust = 1))
  }
}
