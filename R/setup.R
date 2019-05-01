setup <- function(data_train, data_test = NULL, target, weight = NULL, time_var = NULL,
                  family = c("poisson", "gamma", "tweedie"), tweedie_p = NULL,
                  simple_factors = NULL, seed = NULL) {

  stopifnot(inherits(data_train, "data.frame"))
  if(!is.null(data_test)) stopifnot(inherits(data_test), "data.frame")

  family <- match.arg(family)
  if(family %in% c("poisson", "gamme")) stopifnot(is.null(tweedie_p))
  if(family %in% c("tweedie")) stopifnot(!is.null(tweedie_p) && tweedie_p >= 1 && tweedie_p <= 2)

  list(
    data_train = data_train,
    data_test = data_test,
    target = target,
    weight = weight,
    time_var = time_var,
    family = family,
    tweedie_p = tweedie_p,
    simple_factors = simple_factors,
    seed = seed
  )
}

#setup(mtcars, "my_target", family = "poisson")
