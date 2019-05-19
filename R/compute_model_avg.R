compute_model_avg <- function(x, x_betas) {

  intercept_row <- x_betas %>% dplyr::filter(factor == "(Intercept)")
  intercept_estimate <- intercept_row$estimate[[1]]

  # when x_betas contains intercept only, variable is either NOT predictor, or an offset
  if(nrow(x_betas) == 1) {
    orig_levels <- attr(x, "orig_levels")

    if(inherits(x, "offset")) {

      estimate <- unname(attr(x, "mapping"))

      model_avg_df <- dplyr::bind_cols(
        orig_level = orig_levels,
        estimate = estimate,
        model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate),
        model_avg_lin_nonrescaled = intercept_estimate + estimate,
        model_avg_pred_rescaled = exp(estimate),
        model_avg_lin_rescaled = estimate
      ) %>%
      dplyr::mutate(geom_text_label = paste0(round((model_avg_pred_rescaled - 1) * 100), "%"))

    } else {

      model_avg_df <- dplyr::bind_cols(
        orig_level = orig_levels,
        model_avg_pred_nonrescaled = rep(NA_real_, length(orig_levels)),
        model_avg_lin_nonrescaled = rep(NA_real_, length(orig_levels)),
        model_avg_pred_rescaled = rep(NA_real_, length(orig_levels)),
        model_avg_lin_rescaled = rep(NA_real_, length(orig_levels)),
        geom_text_label = rep("", length(orig_levels))
      )

    }

    return(model_avg_df)
  }

  main_rows <- x_betas %>% dplyr::filter(factor != "(Intercept)") %>%
    dplyr::select(actual_level, estimate)

  if(inherits(x, "custom_factor")) {
    mapping <- attr(x, "mapping")

    model_avg_df <- dplyr::bind_cols(
      orig_level = names(mapping),
      actual_level = as.character(unname(mapping))
      ) %>%
      dplyr::left_join(main_rows, by = c("actual_level")) %>%
      dplyr::mutate(estimate = dplyr::coalesce(estimate, 0)) %>%
      dplyr::mutate(
        model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate),
        model_avg_lin_nonrescaled = intercept_estimate + estimate,
        model_avg_pred_rescaled = exp(estimate),
        model_avg_lin_rescaled = estimate
      ) %>%
      dplyr::select(-actual_level, -estimate)

  } else if(inherits(x, "variate")) {

    estimates <- main_rows$estimate
    mapping_df <- attr(x, "mapping")
    orthogonal_x_vals <- mapping_df %>%
      dplyr::select(dplyr::contains("orthogonal_degree_"))

    linear_nonrescaled <- rep(intercept_estimate, nrow(mapping_df))
    for(i in seq_along(estimates)) {
      linear_nonrescaled <- linear_nonrescaled + (orthogonal_x_vals[[i]] * estimates[[i]])
    }

    base_level <- attr(x, "base_level")
    base_level_ind <- which(as.character(mapping_df$orig_level) == base_level)
    base_level_val <- linear_nonrescaled[[base_level_ind]]

    model_avg_df <- dplyr::bind_cols(
      orig_level = mapping_df$orig_level,
      actual_level = mapping_df$actual_level
    ) %>%
    dplyr::mutate(
      model_avg_pred_nonrescaled = exp(linear_nonrescaled),
      model_avg_lin_nonrescaled = linear_nonrescaled,
      model_avg_pred_rescaled = exp(linear_nonrescaled - base_level_val),
      model_avg_lin_rescaled = linear_nonrescaled - base_level_val
    ) %>%
    dplyr::select(-actual_level)

  } else if(inherits(x, "interaction")) {
    mapping <- attr(x, "mapping")

    model_avg_df <- dplyr::bind_cols(
      orig_level = names(mapping),
      actual_level = unname(mapping),
      intercept_estimate = rep(intercept_estimate, length(mapping))
    ) %>%
    dplyr::left_join(main_rows, by = c("actual_level")) %>%
    dplyr::rename(interaction_estimate = estimate) %>%
    dplyr::mutate(interaction_estimate = coalesce(interaction_estimate, 0)) %>%
    tidyr::separate(orig_level, into = c("main1", "main2"), sep = "\\.", remove = FALSE) %>%
    dplyr::left_join(main_rows, by = c("main1" = "actual_level")) %>%
    dplyr::rename(main1_estimate = estimate) %>%
    dplyr::mutate(main1_estimate = coalesce(main1_estimate, 0)) %>%
    dplyr::left_join(main_rows, by = c("main2" = "actual_level")) %>%
    dplyr::rename(main2_estimate = estimate) %>%
    dplyr::mutate(main2_estimate = coalesce(main2_estimate, 0)) %>%
    dplyr::mutate(
      model_avg_pred_nonrescaled =
        exp(intercept_estimate) * exp(main1_estimate) * exp(main2_estimate) * exp(interaction_estimate),
      model_avg_lin_nonrescaled =
        intercept_estimate + main1_estimate + main2_estimate + interaction_estimate,
      model_avg_pred_rescaled =
        exp(main1_estimate) * exp(main2_estimate) * exp(interaction_estimate),
      model_avg_lin_rescaled =
        main1_estimate + main2_estimate + interaction_estimate
    ) %>%
    dplyr::select(orig_level, starts_with("model_avg"))

  } else if(inherits(x, "simple_factor")) {
    model_avg_df <- tibble::tibble(
      orig_level = attr(x, "orig_levels")
    ) %>%
    dplyr::left_join(main_rows, by = c("orig_level" = "actual_level")) %>%
    dplyr::mutate(estimate = dplyr::coalesce(estimate, 0)) %>%
    dplyr::mutate(
      model_avg_pred_nonrescaled = exp(intercept_estimate) * exp(estimate),
      model_avg_lin_nonrescaled = intercept_estimate + estimate,
      model_avg_pred_rescaled = exp(estimate),
      model_avg_lin_rescaled = estimate
    ) %>%
    dplyr::select(-estimate)
  }

  model_avg_df %>%
    dplyr::mutate(orig_level = as.character(orig_level)) %>%
    dplyr::mutate(geom_text_label = paste0(round((model_avg_pred_rescaled - 1) * 100), "%"))
}
