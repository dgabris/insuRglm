setup <- function(data_train, data_test = NULL, target, weight = NULL,
                  family = c("poisson", "gamma", "tweedie"), tweedie_p = NULL,
                  simple_factors = NULL, seed = NULL) {

  stopifnot(inherits(data_train, "data.frame"))
  stopifnot(target %in% colnames(data_train))

  if(is.null(simple_factors)) {
    simple_factors <- setdiff(names(data_train), c(target, weight))
  } else {
    simple_factors <- unique(simple_factors)
  }

  stopifnot(all(simple_factors %in% colnames(data_train)))
  stopifnot(all(vapply(data_train[simple_factors], class, character(1)) == "factor"))

  if(!is.null(data_test)) {
    stopifnot(inherits(data_test, "data.frame"))
    stopifnot(target %in% colnames(data_test))
    stopifnot(all(simple_factors %in% colnames(data_test)))
    stopifnot(all(vapply(data_test[simple_factors], class, character(1)) == "factor"))
  }

  family <- match.arg(family)
  if(family %in% c("poisson", "gamma")) stopifnot(is.null(tweedie_p))
  if(family %in% c("tweedie")) stopifnot(!is.null(tweedie_p) && tweedie_p >= 1 && tweedie_p <= 2)

  tweedie_p <- if(is.null(tweedie_p)) 0 else tweedie_p
  family <- switch(
    family,
    poisson = poisson(link = "log"),
    gamma = Gamma(link = "log"),
    tweedie = statmod::tweedie(var.power = tweedie_p, link.power = 0)
  )

  for(var in simple_factors) {
    data_train[[var]] <- simple_factor(data_train[[var]], data_train[[weight]])
    data_test[[var]] <- simple_factor(data_test[[var]], data_test[[weight]])
  }

  setup <- structure(
    list(
      target = target,
      weight = weight,
      family = family,
      simple_factors = simple_factors,
      seed = seed,
      data_train = tibble::as.tibble(data_train),
      data_test = tibble::as.tibble(data_test),
      current_model = list(predictors = NULL)
    ),
    class = "setup"
  )

  print("Setup - OK")
  print("")
  basic_summary(setup)

  setup
}
