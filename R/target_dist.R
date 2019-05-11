target_dist <- function(setup, weighted = FALSE, without_zero = FALSE,
                        lower_limit = NULL, upper_limit = NULL, ...) {

  stopifnot(inherits(setup, "setup"))
  if(!is.null(lower_limit)) stopifnot(is.numeric(lower_limit) || is.integer(lower_limit))
  if(!is.null(upper_limit)) stopifnot(is.numeric(upper_limit) || is.integer(upper_limit))

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

  g <- ggplot2::ggplot(data = combined_df, ggplot2::aes(x = train_test, y = target)) +
    ggplot2::geom_violin(ggplot2::aes(weight = weight, fill = train_test), ...)

  if(!is.null(lower_limit) || !is.null(upper_limit)) {
    lower_limit <- if(is.null(lower_limit)) pmin(min(train_target), min(test_target)) else lower_limit
    upper_limit <- if(is.null(upper_limit)) pmax(max(train_target), max(test_target)) else upper_limit

    g <- g + ggplot2::coord_cartesian(ylim = c(lower_limit, upper_limit))
  }

  g

}
