target_dist <- function(setup, ...) {
  stopifnot(inherits(setup, "setup"))

  train <- setup$data_train %>%
    mutate(train_test = "train")

  if(!is.null(setup$data_test)) {
    test <- setup$data_test %>%
      mutate(train_test = "test")

  } else {
    test <- NULL

  }

  combined_df <- dplyr::bind_rows(train, test)

  target <- setup$target
  target_sym <- rlang::sym(target)

  ggplot2::ggplot(data = combined_df, ggplot2::aes(x = train_test, y = !!target_sym)) +
    ggplot2::geom_violin(ggplot2::aes(fill = train_test), ...)

}
