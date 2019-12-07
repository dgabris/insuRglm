#' Explore correlations
#'
#' Helps to explore associations (correlations between categorical variables) in the data or in the current (last) insuRglm model.
#' It can be either through visual or tabular form. The metric used for strength of association is Cramer's V.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param type Character scalar. Either \code{visual} or \code{tabular}.
#'
#' @return Either a dataframe or a ggplot2 chart.
#' @export
#'
#' @seealso \code{\link{explore_data}}, \code{\link{explore_target}}
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
#' explore_corr(setup, type = 'visual')
#' explore_corr(setup, type = 'tabular')
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat) %>%
#'   factor_add(veh_age) %>%
#'   factor_add(veh_value) %>%
#'   model_fit()
#'
#' explore_corr(modeling, type = 'visual')
#' explore_corr(modeling, type = 'tabular')
#'

explore_corr <- function(setup, type = c("visual", "tabular")) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  type <- match.arg(type)

  if(inherits(setup, "modeling")) {
    simple_factors <- setup$current_model$predictors
  } else {
    simple_factors <- setup$simple_factors
  }

  train <- setup$data_train[simple_factors]

  combinations_df <-
    tidyr::crossing(var1 = simple_factors, var2 = simple_factors) %>%
    dplyr::distinct()

  cramers_v_vector <- vector(length = nrow(combinations_df))

  for(i in 1:nrow(combinations_df)) {
    first_col <- combinations_df[[i, 1]]
    second_col <- combinations_df[[i, 2]]

    cramers_v_vector[[i]] <- cramers_v(train[[first_col]], train[[second_col]])
  }

  assoc_df <- cbind(combinations_df, cramers_v = cramers_v_vector)

  deduped_assoc_df <- assoc_df %>%
    dplyr::mutate(var_both = purrr::pmap(list(var1, var2), function(x, y) paste0(sort(c(x, y)), collapse = " "))) %>%
    tidyr::unnest(cols = c(var_both)) %>%
    dplyr::distinct(var_both, cramers_v) %>%
    tidyr::separate(var_both, into = c("var1", "var2"), sep = " ") %>%
    dplyr::select(var1, var2, cramers_v)

  if(type == "tabular") {
    deduped_assoc_df %>%
        dplyr::filter(!dplyr::near(cramers_v, 1)) %>%
        dplyr::arrange(dplyr::desc(cramers_v)) %>%
        dplyr::mutate(cramers_v = round(cramers_v, digits = 2))

  } else {
    deduped_assoc_df %>%
      dplyr::mutate(cramers_v = dplyr::if_else(var1 == var2, NA_real_, cramers_v)) %>%
      dplyr::mutate(var1 = as.factor(var1)) %>%
      dplyr::mutate(var2 = factor(var2, levels = rev(levels(var1)))) %>%
      ggplot2::ggplot(ggplot2::aes(x = var1, y = var2, fill = cramers_v)) +
      ggplot2::geom_tile(color = 'black') +
      ggplot2::ggtitle("Correlation Matrix") +
      ggplot2::labs(x = NULL, y = NULL, color = "Cramer's V") +
      ggplot2::scale_fill_gradientn(colours = c("white", "orange", "red"), values = c(0, 0.3, 1)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.45),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
}
