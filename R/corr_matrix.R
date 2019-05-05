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
    dplyr::distinct()

  cramers_v_vector <- vector(length = nrow(combinations_df))

  for(i in 1:nrow(combinations_df)) {
    first_col <- combinations_df[[i, 1]]
    second_col <- combinations_df[[i, 2]]

    cramers_v_vector[[i]] <- cramers_v(train[[first_col]], train[[second_col]])
  }

  assoc_df <- cbind(combinations_df, cramers_v = cramers_v_vector)

  deduped_assoc_df <- assoc_df %>%
    dplyr::mutate(var_both = purrr::pmap(list(var1, var2), function(x, y) paste0(sort(c(x, y)), collapse = " "))) %>%
    tidyr::unnest() %>%
    dplyr::distinct(var_both, cramers_v) %>%
    tidyr::separate(var_both, into = c("var1", "var2"), sep = " ") %>%
    dplyr::select(var1, var2, cramers_v)

  if(type == "table") {
    deduped_assoc_df %>%
        dplyr::filter(!dplyr::near(cramers_v, 1)) %>%
        dplyr::arrange(dplyr::desc(cramers_v)) %>%
        dplyr::mutate(cramers_v = round(cramers_v, digits = 2))

  } else {
    deduped_assoc_df %>%
      dplyr::mutate(cramers_v = dplyr::if_else(var1 == var2, NA_real_, cramers_v)) %>%
      dplyr::mutate(var1 = as.factor(var1)) %>%
      dplyr::mutate(var2 = factor(var2, levels = rev(levels(var1)))) %>%
      ggplot2::ggplot(ggplot2::aes(x = var1, y = var2, color = cramers_v)) +
      ggplot2::geom_point(size = 5) +
      ggplot2::ggtitle("Correlation Matrix") +
      ggplot2::labs(x = NULL, y = NULL, color = "Cramer's V") +
      ggplot2::scale_color_gradientn(colours = c("white", "orange", "red"), values = c(0, 0.3, 1)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.45),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
}
