twoway_plot <- function(df) {
  stopifnot(inherits(df, "data.frame"))

  x_cols <- names(df)[c(1, 2)]
  x1_sym <- rlang::sym(x_cols[[1]])
  x2_sym <- rlang::sym(x_cols[[2]])

  gather_cols <- rlang::syms(names(df)[-c(1, 2)])

  combined_df <- df %>%
    tidyr::gather(key = type, value = target, !!!gather_cols)

  weight_df <- combined_df %>%
    dplyr::filter(type == "weight_sum") %>%
    dplyr::mutate(facet = "weight")

  target_df <- combined_df %>%
    dplyr::filter(type != "weight_sum") %>%
    dplyr::mutate(facet = "target")

  combined_df %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x1_sym, y = target, group = !!x2_sym)) +
    ggplot2::facet_grid(facet ~ ., scale = "free") +
    ggplot2::geom_point(data = target_df, ggplot2::aes(color = !!x2_sym), shape = 15, size = 2) +
    ggplot2::geom_line(data = target_df, ggplot2::aes(color = !!x2_sym), size = 1.05) +
    ggplot2::geom_bar(data = weight_df, ggplot2::aes(fill = !!x2_sym), stat = "identity", color = "black") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::ggtitle(label = x_cols[[1]]) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

}
