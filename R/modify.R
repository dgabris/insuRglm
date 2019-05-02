modify <- function(setup, ...) {
  dots <- enexprs(...)

  setup$data_train <- setup$data_train %>%
    mutate(!!!dots)
}
