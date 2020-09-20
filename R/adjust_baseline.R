# baseline for other factors needs to be adjusted in the presence of orthogonal variates,
# since their base value is not 0
adjust_baseline <- function(betas, predictor_attrs) {

  stopifnot(inherits(betas, "data.frame"))
  stopifnot(inherits(predictor_attrs, "list"))

  intercept <- betas[[1, "estimate"]]

  variates <- names(Filter(x = predictor_attrs, f = function(x) x$class[[1]] == "variate"))

  if(length(variates) == 0) {
    return(intercept)
  }

  adjustments <- numeric(length(variates))
  for(i in seq_along(variates)) {
    this_predictor <- variates[[i]]
    this_attrs <- predictor_attrs[[this_predictor]]
    this_mapping <- this_attrs$mapping
    this_base_lvl <- this_attrs$base_level

    this_betas <- betas %>%
      dplyr::filter(factor == this_predictor) %>%
      dplyr::pull(estimate)

    orthogonal_x_vals <- this_mapping %>%
      dplyr::filter(as.character(orig_level) == this_base_lvl) %>%
      dplyr::select(dplyr::contains("orthogonal_degree_"))

    stopifnot(nrow(orthogonal_x_vals) == 1)
    orthogonal_x_vals <- unlist(orthogonal_x_vals, use.names = FALSE)

    adjustments[[i]] <- sum(this_betas * orthogonal_x_vals)
  }

  intercept + sum(adjustments)
}
