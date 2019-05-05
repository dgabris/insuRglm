factor_modify <- function(setup, ...) {
  dots <- rlang::enexprs(...)

  setup$data_train <- setup$data_train %>%
    dplyr::mutate(!!!dots)

  setup$data_test <- setup$data_test %>%
    dplyr::mutate(!!!dots)

  setup
}
