require("insuRglm")
require("dplyr")

data("freq_train")
data("sev_train")
data("bc_train")

data("freq_test")
data("sev_test")
data("bc_test")

no_print <- function(expr) {
  invisible(capture.output(expr))
}

freq_args <- list(
  data_train = freq_train,
  data_test = freq_test,
  target = "freq",
  offset = "exposure",
  family = "poisson",
  keep_cols = c("pol_nbr", "premium")
)

sev_args <- list(
  data_train = sev_train,
  data_test = sev_test,
  target = "sev",
  weight = "numclaims",
  family = "gamma",
  keep_cols = c("pol_nbr", "exposure", "premium")
)

bc_args <- list(
  data_train = bc_train,
  data_test = bc_test,
  target = "bc",
  weight = "exposure",
  family = "tweedie",
  tweedie_p = 1.5,
  keep_cols = c("pol_nbr", "exposure", "premium")
)

no_print(freq_setup <- suppressMessages({do.call(setup, freq_args)}))
no_print(sev_setup <- do.call(setup, sev_args))
no_print(bc_setup <- do.call(setup, bc_args))
