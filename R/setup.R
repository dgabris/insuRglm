setup <- function(data_train, data_test = NULL, target, weight = NULL, time_var = NULL,
                  family = c("poisson", "gamma", "tweedie"), tweedie_p = NULL,
                  simple_factors = NULL, seed = NULL) {

  simple_factors <- unique(c(simple_factors, time_var))

  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(target %in% colnames(data_train))
  stopifnot(all(simple_factors %in% colnames(data_train)))
  stopifnot(all(vapply(data_train[simple_factors], class, character(1)) == "factor"))

  if(!is.null(data_test)) {
    stopifnot(inherits(data_test, "data.frame"))
    stopifnot(target %in% colnames(data_test))
    stopifnot(all(simple_factors %in% colnames(data_test)))
    #stopifnot(all(vapply(data_test[simple_factors], class, character(1)) == "factor"))
  }

  family <- match.arg(family)
  if(family %in% c("poisson", "gamma")) stopifnot(is.null(tweedie_p))
  if(family %in% c("tweedie")) stopifnot(!is.null(tweedie_p) && tweedie_p >= 1 && tweedie_p <= 2)

  if(family == "gamma") family <- "Gamma"

  if(length(simple_factors) == 1 && simple_factors == time_var) {
    simple_factors <- setdiff(names(data_train), c(target, weight))
  }

  for(var in simple_factors) {
    data_train[[var]] <- simple_factor(data_train[[var]], data_train[[weight]])
  }

  setup <- structure(
    list(
      data_train = tibble::as.tibble(data_train),
      data_test = tibble::as.tibble(data_test),
      target = target,
      predictors = time_var,
      weight = weight,
      time_var = time_var,
      family = family,
      tweedie_p = tweedie_p,
      simple_factors = simple_factors,
      seed = seed
    ),
    class = "setup"
  )

  print("Setup - OK")
  print("")
  basic_summary(setup)

  setup
}
