target_dist <- function(setup, weighted = TRUE, without_zero = FALSE, cap_at = NULL, ...) {
  stopifnot(inherits(setup, "setup"))
  if(!is.null(cap_at)) stopifnot(is.numeric(cap_at) || is.integer(cap_at))

  test_exists <- !is.null(setup$data_test)
  weight_exists <- !is.null(setup$weight)

  train_target <- setup$data_train[[setup$target]]
  test_target <- if(test_exists) setup$data_test[[setup$target]] else NULL

  train_weight <- rep(1, nrow(setup$data_train))
  test_weight <- if(test_exists) rep(1, nrow(setup$data_test)) else NULL

  if(weighted && weight_exists) {
    train_weight <- setup$data_train[[setup$weight]]
    test_weight <- if(test_exists) setup$data_test[[setup$weight]]
  }

  sum_train_weight <- sum(train_weight)
  train_weight <- train_weight / sum_train_weight

  sum_test_weight <- if(test_exists) sum(test_weight)
  test_weight <- if(test_exists) test_weight / sum_test_weight

  train_df <- tibble::tibble(train_test = "train", target = train_target, weight = train_weight)
  test_df <- if(test_exists) tibble::tibble(train_test = "test", target = test_target, weight = test_weight) else NULL

  combined_df <- dplyr::bind_rows(train_df, test_df)

  if(without_zero) {
    combined_df <- combined_df %>%
      dplyr::filter(!near(target, 0))
  }

  if(!is.null(cap_at)) {
    combined_df <- combined_df %>%
      dplyr::mutate(target = if_else(target > cap_at, cap_at, target))
  }

  ggplot2::ggplot(data = combined_df, ggplot2::aes(x = train_test, y = target)) +
    ggplot2::geom_violin(ggplot2::aes(weight = weight, fill = train_test), ...)

}
