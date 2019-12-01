oneway_plot <- function(df, colors = NULL, label_prefix = NULL) {
  stopifnot(inherits(df, "data.frame"))

  x_col <- names(df)[1]
  x_sym <- rlang::sym(x_col)

  keep_cols <- c(x_col, "geom_text_label")
  gather_cols <- setdiff(names(df), keep_cols)
  gather_syms <- rlang::syms(gather_cols)

  combined_df <- df %>%
    tidyr::gather(key = type, value = target, !!!gather_syms) %>%
    dplyr::filter(!is.na(target))

  weight_df <- combined_df %>%
    dplyr::filter(type == "weight_sum") %>%
    dplyr::mutate(facet = "weight")

  target_df <- combined_df %>%
    dplyr::filter(type != "weight_sum") %>%
    dplyr::mutate(facet = "target")

  # keeping relativity percentage labels only on model predictions at base levels
  if("pred_base_levels" %in% names(df)) {
    target_df <- target_df %>%
      dplyr::mutate(geom_text_label = dplyr::if_else(type != "pred_base_levels", "", geom_text_label))
  }

  g <- combined_df %>%
  ggplot2::ggplot(ggplot2::aes(x = !!x_sym, y = target, group = type)) +
    ggplot2::facet_grid(facet ~ ., scale = "free") +
    ggplot2::geom_line(data = target_df, ggplot2::aes(color = type), size = 1.05) +
    ggplot2::geom_point(data = target_df, ggplot2::aes(fill = type), color = "black", shape = 22, size = 2) +
    ggplot2::geom_text(data = target_df, ggplot2::aes(label = geom_text_label), hjust = 1.2, size = 3) +
    ggplot2::geom_bar(data = weight_df, stat = "identity", fill = "#F0E442", color = "black") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::ggtitle(label = paste0(label_prefix, x_col)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if(!is.null(colors)) {
    g +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    g
  }

}
