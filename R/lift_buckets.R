lift_buckets <- function(setup, model_list, buckets, method = c("train", "crossval", "test"),
                         cv_folds, cv_stratified) {

  stopifnot(inherits(setup, "setup"))
  stopifnot(is.numeric(buckets) || is.integer(buckets))
  method <- match.arg(method)
  stopifnot(is.numeric(cv_folds) || is.integer(cv_folds))
  if(method == "test") stopifnot(!is.null(setup$data_test))
  stopifnot(typeof(model_list) == "list")

  if(method %in% c("train", "crossval")) {
    actual <- setup$data_train[[setup$target]]
    weight <- setup$data_train[[setup$weight]]
  } else {
    actual <- setup$data_test[[setup$target]]
    weight <- setup$data_test[[setup$weight]]
  }

  if(method == "crossval") {
    n_model_fits <- length(model_list) * cv_folds

    message(
      paste0(
        "This may take a while. ", n_model_fits,
        " models need to be fitted."
      ))
  }

  lift_buckets_list <- list()
  for(model_name in names(model_list)) {
    model <- model_list[[model_name]]

    if(method == "train") {
      expected <- model$train_predictions

    } else if(method == "crossval") {
      expected <- crossval_predict(setup$data_train, model, cv_folds, cv_stratified)

    } else {
      expected <- model$test_predictions
    }

    lift_buckets_list[[model_name]] <- dplyr::bind_cols(
      weight = weight,
      actual = actual,
      expected = expected
    ) %>%
      dplyr::mutate(
        bucket = findInterval(
          expected,
          quantile(expected, probs = seq(0, 1, by = 1 / buckets)),
          all.inside = TRUE
        )
      ) %>%
      dplyr::group_by(bucket) %>%
      dplyr::summarize(
        actual = sum(actual * weight) / sum(weight),
        expected = sum(expected * weight) / sum(weight)
      ) %>%
      dplyr::ungroup() %>%
      tidyr::gather(key = type, value = target, actual, expected)

  }

  lift_buckets_list
}
