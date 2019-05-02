lift_chart <- function(something) {
  # TODO - determine what will be the input and change function body accordingly

  deciles <- data_w_predictions %>%
    mutate(bucket = findInterval(
      pred_target,
      quantile(pred_target, probs = seq(0, 1, by = 1 / num_buckets)),
      all.inside = TRUE
    )) %>%
    group_by(bucket) %>%
    summarize(
      `Actual` = sum(fire_loss) / sum(pol_loc_premium),
      `Predicted` = sum(pred_target * !!rlang::sym(weight)) / sum(!!rlang::sym(weight))
    ) %>%
    ungroup() %>%
    tidyr::gather(key = type, value = "loss_ratio", `Actual`, `Predicted`)

  deciles %>%
    ggplot(aes(x = as.factor(bucket), y = loss_ratio)) +
    geom_bar(aes(fill = type), stat = "identity", position = "dodge") +
    labs(x = "Bucket", y = "Loss Ratio", fill = "Type")
}
