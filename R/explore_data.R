#' Explore the dataset
#'
#' Helps to explore data in either visual or tabular form. The view can also be one or two-dimensional.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param type Character scalar. Either \code{visual} or \code{tabular}.
#' @param factors Character vector. Names of selected potential predictors to limit the scope of results.
#' @param by Character scalar. Name of potential predictor that will constitute the second dimension.
#'
#' @return Either list of dataframes or list of ggplot2 charts.
#' @export
#'
#' @seealso \code{\link{explore_target}}, \code{\link{explore_corr}}
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = sev_train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' explore_data(setup, type = "tabular")
#' explore_data(setup, type = "visual")
#' explore_data(setup, type = "tabular", by = "pol_yr")
#' explore_data(setup, type = "visual", by = "pol_yr")
#'

explore_data <- function(setup, type = c("visual", "tabular"), factors = NULL, by = NULL) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  type <- match.arg(type)

  if(is.null(factors)) {
    factors <- setup$simple_factors
  } else {
    if(!is.character(factors)) stop("'factors' must be a character vector")
  }

  if(!is.null(by)) {
    if(!(is.character(by) && length(by))) stop("'by' must be a character scalar")
    if(!by %in% setup$simple_factors) stop("'by' is not present in the dataset provided to setup object")
    factors <- setdiff(factors, by)
  }

  train <- setup$data_train
  weight_vector <- train[[setup$weight]]
  target_sym <- rlang::sym(setup$target)

  factors_list <- as.list(factors) %>%
    purrr::set_names(factors) %>%
    lapply(function(x) {
      x <- c(x, by)
      x
    }) %>%
    lapply(function(x) {
      x_syms <- rlang::syms(x)

      dplyr::bind_cols(
        train,
        .weight = weight_vector
      ) %>%
        dplyr::group_by(!!!x_syms) %>%
        dplyr::summarize(
          weight_sum = sum(.weight),
          target_sum = sum(!!target_sym),
          target_avg = sum(!!target_sym * .weight) / sum(.weight)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!!x_syms)
    })

  if(type == "tabular") {
    return(factors_list)
  }

  if(is.null(by)) {

    plot_list <- factors_list %>%
      lapply(function(x) {
        x %>%
          dplyr::select(-target_sum) %>%
          dplyr::rename(`Obs. Avg.` = target_avg) %>%
          dplyr::mutate(geom_text_label = "") %>%
          oneway_plot(colors = c("#CC79A7"))
      })

  } else {

    plot_list <- factors_list %>%
      lapply(function(x) {
        x %>%
          dplyr::select(-target_sum) %>%
          twoway_plot()
      })

    plot_list[[by]] <- NULL

  }

  plot_list
}
