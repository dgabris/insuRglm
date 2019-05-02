setup <- function(data_train, data_test = NULL, target, weight = NULL, time_var = NULL,
                  family = c("poisson", "gamma", "tweedie"), tweedie_p = NULL,
                  simple_factors = NULL, seed = NULL) {

  simple_factors <- c(simple_factors, time_var)

  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(target %in% colnames(data_train))
  stopifnot(all(simple_factors %in% colnames(data_train)))
  stopifnot(all(vapply(data_train[simple_factors], class) == "factor"))

  if(!is.null(data_test)) {
    stopifnot(inherits(data_test), "data.frame")
    stopifnot(target %in% colnames(data_test))
    stopifnot(all(simple_factors %in% colnames(data_test)))
    stopifnot(all(vapply(data_test[simple_factors], class) == "factor"))
  }

  family <- match.arg(family)
  if(family %in% c("poisson", "gamma")) stopifnot(is.null(tweedie_p))
  if(family %in% c("tweedie")) stopifnot(!is.null(tweedie_p) && tweedie_p >= 1 && tweedie_p <= 2)

  structure(
    list(
      data_train = data_train,
      data_test = data_test,
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
}
