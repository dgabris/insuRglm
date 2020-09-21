oneway_plot <- function(df, colors = NULL, label_prefix = NULL) {
  stopifnot(inherits(df, "data.frame"))

  x_col <- names(df)[1]
  x_sym <- rlang::sym(x_col)

  keep_cols <- c(x_col, "weight_sum", "geom_text_label")
  gather_cols <- setdiff(names(df), keep_cols)
  gather_syms <- rlang::syms(gather_cols)

  longer_df <- df %>%
    tidyr::pivot_longer(cols = gather_cols, names_to = "type", values_to = "target") %>%
    dplyr::filter(!is.na(target))

  # keeping relativity percentage labels only on model predictions at base levels
  if("Model Parameters" %in% names(df)) {
    longer_df <- longer_df %>%
      dplyr::mutate(geom_text_label = dplyr::if_else(type != "Model Parameters", "", geom_text_label))
  }

  target_plot <- longer_df %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x_sym, y = target, group = type)) +
    ggplot2::geom_line(ggplot2::aes(color = type), size = 1.05) +
    ggplot2::geom_point(ggplot2::aes(fill = type), color = "black", shape = 22, size = 2) +
    ggplot2::geom_text(ggplot2::aes(label = geom_text_label), hjust = 1.2, size = 3) +
    ggplot2::labs(color = "Type", fill = "Type") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = "Target") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7)
    )

  if(!is.null(colors)) {
    target_plot <- target_plot +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  }

  weight_plot <- df %>%
    dplyr::select(!!x_sym, weight_sum) %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x_sym, y = weight_sum)) +
    ggplot2::geom_bar(stat = "identity", fill = "#F0E442", color = "black") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = "Weight") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.text = ggplot2::element_text(size = 7)
    )

  target_plot +
    weight_plot +
    patchwork::plot_layout(nrow = 2, ncol = 1, heights = c(2, 1), guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(label_prefix, x_col),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.45)
      )
    )
}
