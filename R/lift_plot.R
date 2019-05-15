lift_plot <- function(lift_buckets, title) {

  stopifnot(inherits(lift_buckets, "data.frame"))

  lift_buckets %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(bucket), y = target)) +
    ggplot2::geom_bar(ggplot2::aes(fill = type), stat = "identity", position = "dodge") +
    ggplot2::labs(x = "Bucket", y = "Target", fill = "Type") +
    ggplot2::ggtitle(paste0("Lift Chart - ", title)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45)
    )

}
