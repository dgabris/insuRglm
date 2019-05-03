lift_chart <- function(something) {
  # TODO - determine what will be the input and change function body accordingly

  deciles <- data_w_predictions %>%
    dplyr::mutate(bucket = findInterval(
      pred_target,
      quantile(pred_target, probs = seq(0, 1, by = 1 / num_buckets)),
      all.inside = TRUE
    )) %>%
    dplyr::group_by(bucket) %>%
    dplyr::summarize(
      `Actual` = sum(fire_loss) / sum(pol_loc_premium),
      `Predicted` = sum(pred_target * !!rlang::sym(weight)) / sum(!!rlang::sym(weight))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(key = type, value = "loss_ratio", `Actual`, `Predicted`)

  deciles %>%
    ggplot2::ggplot(aes(x = as.factor(bucket), y = loss_ratio)) +
    ggplot2::geom_bar(aes(fill = type), stat = "identity", position = "dodge") +
    ggplot2::labs(x = "Bucket", y = "Loss Ratio", fill = "Type")
}
