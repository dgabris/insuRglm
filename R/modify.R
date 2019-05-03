modify <- function(setup, ...) {
  dots <- rlang::enexprs(...)

  setup$data_train <- setup$data_train %>%
    dplyr::mutate(!!!dots)

  setup
}
