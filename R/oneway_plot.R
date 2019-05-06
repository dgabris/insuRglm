oneway_plot <- function(df, colors) {
  stopifnot(inherits(df, "data.frame"))

  x_col <- names(df)[1]
  x_sym <- rlang::sym(x_col)
  gather_cols <- rlang::syms(names(df)[-1])

  combined_df <- df %>%
    tidyr::gather(key = type, value = target, !!!gather_cols) %>%
    dplyr::filter(!is.na(target))

  weight_df <- combined_df %>%
    dplyr::filter(type == "weight_sum") %>%
    dplyr::mutate(facet = "weight")

  target_df <- combined_df %>%
    dplyr::filter(type != "weight_sum") %>%
    dplyr::mutate(facet = "target")

  combined_df %>%
  ggplot2::ggplot(ggplot2::aes(x = !!x_sym, y = target, group = type)) +
    ggplot2::facet_grid(facet ~ ., scale = "free") +
    ggplot2::geom_point(data = target_df, ggplot2::aes(color = type), shape = 15, size = 2) +
    ggplot2::geom_line(data = target_df, ggplot2::aes(color = type), size = 1.05) +
    ggplot2::geom_bar(data = weight_df, stat = "identity", fill = "#F0E442", color = "black") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::ggtitle(label = x_col) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

}
