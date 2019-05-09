target_dist <- function(setup, weighted = TRUE, without_zero = FALSE, ...) {
  stopifnot(inherits(setup, "setup"))

  train_target <- setup$data_train[[setup$target]]
  train_weight <- setup$data_train[[setup$weight]]

  test_target <- setup$data_test[[setup$target]]
  test_weight <- setup$data_test[[setup$weight]]

  min_weight <- pmin(min(train_weight), min(test_weight))

  if(weighted) {
    train_target <- rep(train_target, train_weight / min_weight)
    test_target <- rep(test_target, test_weight / min_weight)
  }


  train_df <- tibble::tibble(train_test = "train", target = train_target)
  test_df <- if(!is.null(setup$data_test)) tibble::tibble(train_test = "test", target = test_target) else NULL

  combined_df <- dplyr::bind_rows(train_df, test_df)

  if(without_zero) {
    combined_df <- combined_df %>%
      dplyr::filter(!near(target, 0))
  }

  ggplot2::ggplot(data = combined_df, ggplot2::aes(x = train_test, y = target)) +
    ggplot2::geom_violin(ggplot2::aes(fill = train_test), ...)

}
