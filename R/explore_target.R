#' Explore the target variable
#'
#' Helps to explore the distribution of target variable in either visual or tabular form.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param type Character scalar. Either \code{visual} or \code{tabular}.
#' @param weighted Logical scalar. Whether the distribution should be weighted.
#' @param exclude_zero Logical scalar. Whether to exclude records with zero value of target variable.
#' @param lower_quantile Numeric scalar. Lower quantile to include in the distribution analysis.
#' @param upper_quantile Numeric scalar. Upper quantile to include in the distribution analysis.
#' @param n_cuts Integer scalar. Number of cuts in the printed distribution. More cuts means more detailed view.
#'
#' @return Either a ggplot2 chart or vector of values printed to console.
#' @export
#'
#' @seealso \code{\link{explore_data}}, \code{\link{explore_corr}}
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('sev_train')
#'
#' setup <- setup(
#'   data_train = train,
#'   target = 'sev',
#'   weight = 'numclaims',
#'   family = 'gamma',
#'   keep_cols = c('pol_nbr', 'exposure', 'premium')
#' )
#'
#' explore_target(setup)
#' explore_target(setup, type = 'tabular')
#' explore_target(setup, type = 'visual')
#' explore_target(setup, type = 'tabular', exclude_zero = TRUE)
#' explore_target(setup, type = 'visual', lower_quantile = 0.05, upper_quantile = 0.95)
#'

explore_target <- function(setup, type = c('visual', 'tabular'), weighted = TRUE, exclude_zero = FALSE,
                           lower_quantile = 0, upper_quantile = 1, n_cuts = 20)  {

  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  type <- match.arg(type)
  if(!(is.logical(weighted) && length(weighted) == 1)) stop("'weighted' must be a logical scalar")
  if(!(is.logical(exclude_zero) && length(exclude_zero) == 1)) stop("'exclude_zero' must be a logical scalar")

  if(!(is.numeric(lower_quantile) && length(lower_quantile) == 1 && lower_quantile >= 0 && lower_quantile <= 1)) {
    stop("'lower_quantile' must be a numeric scalar with value between 0 and 1")
  }

  if(!(is.numeric(upper_quantile) && length(upper_quantile) == 1 && upper_quantile >= 0 && upper_quantile <= 1)) {
    stop("'upper_quantile' must be a numeric scalar with value between 0 and 1")
  }

  if(lower_quantile > upper_quantile) stop("'lower_quantile' must be less than 'upper_quantile'")

  if(!(is.numeric(n_cuts) && length(n_cuts) == 1 && n_cuts > 0)) stop("'n_cuts' must be an integer scalar greater than zero")
  if(!(n_cuts - as.integer(n_cuts)) == 0) stop("'n_cuts' must be a whole number")

  test_exists <- !is.null(setup$data_test)

  train_target <- setup$data_train[[setup$target]]
  test_target <- if(test_exists) setup$data_test[[setup$target]] else NULL

  train_weight <- rep(1, nrow(setup$data_train))
  test_weight <- if(test_exists) rep(1, nrow(setup$data_test)) else NULL

  if(weighted) {
    train_weight <- setup$data_train[[setup$weight]]
    test_weight <- if(test_exists) setup$data_test[[setup$weight]]
  }
#
#   sum_train_weight <- sum(train_weight_orig)
#   train_weight_norm <- train_weight_orig / sum_train_weight
#
#   sum_test_weight <- if(test_exists) sum(test_weight_orig)
#   test_weight_norm <- if(test_exists) test_weight_orig / sum_test_weight

  train_df <- tibble::tibble(train_test = "train", target = train_target, weight = train_weight)

  if(test_exists) {
    test_df <- tibble::tibble(train_test = "test", target = test_target, weight = test_weight)
  } else {
    test_df <- NULL
  }

  lower_quantile_val <- quantile(train_target, probs = lower_quantile)
  upper_quantile_val <- quantile(train_target, probs = upper_quantile)

  combined_df <- dplyr::bind_rows(train_df, test_df)
  combined_df$train_test <- factor(combined_df$train_test, levels = c('train', if(test_exists) 'test' else NULL))

  if(exclude_zero) {
    combined_df <- combined_df %>%
      dplyr::filter(!dplyr::near(target, 0))
  }

  if(lower_quantile != 0) {
    lower_limit <- quantile(train_target, probs = lower_quantile)
    combined_df <- combined_df %>%
      dplyr::filter(target >= lower_limit)
  } else {
    lower_limit <- min(train_target)
  }

  if(upper_quantile != 1) {
    upper_limit <- quantile(train_target, probs = upper_quantile)
    combined_df <- combined_df %>%
      dplyr::filter(target <= upper_limit)
  } else {
    upper_limit <- max(train_target)
  }

  if(type == 'tabular') {
    dfs <- split(combined_df, combined_df$train_test)
    probs <- seq(from = lower_quantile, to = upper_quantile, length.out = n_cuts + 1)

    result <- dfs %>%
      lapply(function(x) Hmisc::wtd.quantile(x$target, x$weight, probs))

    return(result)
  }

  combined_df <- combined_df %>%
    dplyr::group_by(train_test) %>%
    dplyr::mutate(weight = weight / sum(weight)) %>%
    dplyr::ungroup()

  g <- ggplot2::ggplot(data = combined_df, ggplot2::aes(x = train_test, y = target)) +
    ggplot2::geom_violin(ggplot2::aes(weight = weight, fill = train_test))

  if(!is.null(lower_limit) || !is.null(upper_limit)) {
    lower_limit <- if(is.null(lower_limit)) pmin(min(train_target), min(test_target)) else lower_limit
    upper_limit <- if(is.null(upper_limit)) pmax(max(train_target), max(test_target)) else upper_limit

    g <- g + ggplot2::coord_cartesian(ylim = c(lower_limit, upper_limit))
  }

  g

}
