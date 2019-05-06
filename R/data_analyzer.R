data_analyzer <- function(setup, factors = NULL, second_dim = NULL, type = c("graph", "table")) {
  stopifnot(inherits(setup, "setup"))
  type <- match.arg(type)

  factors <- if(is.null(factors)) setup$simple_factors else factors
  stopifnot(is.character(factors))

  train <- setup$data_train
  weight_vector <- if(is.null(setup$weight)) rep(1, nrow(train)) else train[[setup$weight]]
  target_sym <- rlang::sym(setup$target)


  factors_list <- as.list(factors) %>%
    purrr::set_names(factors) %>%
    lapply(function(x) {
      x <- c(x, second_dim)
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

  if(type == "table") {
    return(factors_list)
  }

  if(is.null(second_dim)) {

    plot_list <- factors_list %>%
      lapply(function(x) {
        x %>%
          dplyr::select(-target_sum) %>%
          oneway_plot(colors = c("#CC79A7"))
      })

  } else {

    plot_list <- factors_list %>%
      lapply(function(x) {
        x %>%
          dplyr::select(-target_sum) %>%
          twoway_plot()
      })

    plot_list[[second_dim]] <- NULL

  }

  plot_list
}
