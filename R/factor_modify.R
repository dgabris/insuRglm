factor_modify <- function(setup, ...) {
  dots <- rlang::enexprs(...)

  setup$data_train <- setup$data_train %>%
    dplyr::mutate(!!!dots)

  if(!is.null(setup$data_test)) {
    setup$data_test <- setup$data_test %>%
      dplyr::mutate(!!!dots)
  }

  setup
}
