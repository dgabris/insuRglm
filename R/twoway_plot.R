twoway_plot <- function(df, label_prefix = NULL, label_suffix = NULL, as_list = FALSE) {
  stopifnot(inherits(df, "data.frame"))

  x_cols <- names(df)[c(1, 2)]
  x1_sym <- rlang::sym(x_cols[[1]])
  x2_sym <- rlang::sym(x_cols[[2]])

  gather_cols <- setdiff(names(df), c(x_cols, "weight_sum"))

  longer_df <- df %>%
    tidyr::pivot_longer(cols = gather_cols, names_to = "type", values_to = "target")

  if(length(gather_cols) > 1) {
    longer_df <- longer_df %>%
      dplyr::mutate(grp = paste0(type, "_", !!x2_sym))

    aes_prep <- ggplot2::aes(x = !!x1_sym, y = target, group = grp)

    if("Fitted Average (current_model)" %in% unique(longer_df$type)) {
      known_values <- c("Observed Average", "Fitted Average (current_model)")
      unknown_value <- setdiff(unique(longer_df$type), known_values)

      shape_mapping <- setNames(c(15, 16, 17), c(known_values, unknown_value))
      linetype_mapping <- setNames(c("solid", "dotted", "dashed"), c(known_values, unknown_value))

      geom_prep <- list(
        ggplot2::geom_line(ggplot2::aes(color = !!x2_sym, linetype = type), size = 1.05),
        ggplot2::geom_point(ggplot2::aes(color = !!x2_sym, shape = type), size = 2.5),
        ggplot2::scale_shape_manual(name = "Type", values = shape_mapping),
        ggplot2::scale_linetype_manual(name = "Type", values = linetype_mapping)
      )

    } else if("Model Parameters (current_model)" %in% unique(longer_df$type)) {
      known_values <- c("Model Parameters (current_model)")
      unknown_value <- setdiff(unique(longer_df$type), known_values)

      shape_mapping <- setNames(c(15, 16), c(known_values, unknown_value))
      linetype_mapping <- setNames(c("solid", "dotted"), c(known_values, unknown_value))

      geom_prep <- list(
        ggplot2::geom_line(ggplot2::aes(color = !!x2_sym, linetype = type), size = 1.05),
        ggplot2::geom_point(ggplot2::aes(color = !!x2_sym, shape = type), size = 2.5),
        ggplot2::scale_shape_manual(name = "Type", values = shape_mapping),
        ggplot2::scale_linetype_manual(name = "Type", values = linetype_mapping)
      )
    } else {
      geom_prep <- list(
        ggplot2::geom_line(ggplot2::aes(color = !!x2_sym, linetype = type), size = 1.05),
        ggplot2::geom_point(ggplot2::aes(color = !!x2_sym, shape = type), size = 2.5),
        ggplot2::scale_shape_manual(name = "Type", values = c(
          "Observed Average" = 15, "Fitted Average" = 16
        )),
        ggplot2::scale_linetype_manual(name = "Type", values = c(
          "Observed Average" = "solid", "Fitted Average" = "dotted"
        ))
      )
    }

  } else {
    aes_prep <- ggplot2::aes(x = !!x1_sym, y = target, group = !!x2_sym)
    geom_prep <- list(
      ggplot2::geom_point(ggplot2::aes(color = !!x2_sym), shape = 15, size = 2),
        ggplot2::geom_line(ggplot2::aes(color = !!x2_sym), size = 1.05)
    )
  }

  target_plot <- longer_df %>%
    ggplot2::ggplot(aes_prep) +
    geom_prep +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = "Target") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7)
    ) +
    ggplot2::guides(color = "none")

  weight_plot <- df %>%
    dplyr::select(!!x1_sym, !!x2_sym, weight_sum) %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x1_sym, y = weight_sum, group = !!x2_sym)) +
    ggplot2::geom_bar(ggplot2::aes(fill = !!x2_sym), stat = "identity", color = "black") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = "Weight") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.text = ggplot2::element_text(size = 7)
    )

  title <- paste0(label_prefix, x_cols[[1]], " x ", x_cols[[2]], label_suffix)

  if(as_list) {
    target_plot <- target_plot +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.45))

    list(target_plot = target_plot, weight_plot = weight_plot)
  } else {
    target_plot +
      weight_plot +
      patchwork::plot_layout(nrow = 2, ncol = 1, heights = c(2, 1), guides = "collect") +
      patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.45)
        )
      )
  }

}
