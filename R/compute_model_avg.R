compute_model_avg <- function(x, x_betas, current_baseline, by = NULL, by_betas = NULL) {

  baseline_estimate <- current_baseline

  # when x_betas contains intercept only, variable is either NOT predictor, or an offset
  if(nrow(x_betas) == 1) {
    orig_levels <- attr(x, "orig_levels")

    if(inherits(x, "offset")) {

      estimate <- unname(attr(x, "mapping"))

      model_avg_df <- dplyr::bind_cols(
        orig_level = orig_levels,
        estimate = estimate,
        model_avg_pred_nonrescaled = exp(baseline_estimate) * exp(estimate),
        model_avg_lin_nonrescaled = baseline_estimate + estimate,
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
        model_avg_pred_nonrescaled = exp(baseline_estimate) * exp(estimate),
        model_avg_lin_nonrescaled = baseline_estimate + estimate,
        model_avg_pred_rescaled = exp(estimate),
        model_avg_lin_rescaled = estimate
      ) %>%
      dplyr::select(-actual_level, -estimate)

  } else if(inherits(x, "variate")) {

    estimates <- main_rows$estimate
    mapping_df <- attr(x, "mapping")
    base_level <- attr(x, "base_level")

    baseline_x_vals <- mapping_df %>%
      dplyr::filter(as.character(orig_level) == base_level) %>%
      dplyr::select(dplyr::contains("orthogonal_degree_"))

    stopifnot(nrow(baseline_x_vals) == 1)
    baseline_x_vals <- unlist(baseline_x_vals, use.names = FALSE)

    existing_adjustment <- sum(estimates * baseline_x_vals)

    # baseline already includes adjustment from this variate, we need to remove it
    this_baseline <- baseline_estimate - existing_adjustment

    orthogonal_x_vals <- mapping_df %>%
      dplyr::select(dplyr::contains("orthogonal_degree_"))

    linear_nonrescaled <- rep(this_baseline, nrow(mapping_df))
    for(i in seq_along(estimates)) {
      linear_nonrescaled <- linear_nonrescaled + (orthogonal_x_vals[[i]] * estimates[[i]])
    }

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
      baseline_estimate = rep(baseline_estimate, length(mapping))
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
        exp(baseline_estimate) * exp(main1_estimate) * exp(main2_estimate) * exp(interaction_estimate),
      model_avg_lin_nonrescaled =
        baseline_estimate + main1_estimate + main2_estimate + interaction_estimate,
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
      model_avg_pred_nonrescaled = exp(baseline_estimate) * exp(estimate),
      model_avg_lin_nonrescaled = baseline_estimate + estimate,
      model_avg_pred_rescaled = exp(estimate),
      model_avg_lin_rescaled = estimate
    ) %>%
    dplyr::select(-estimate)
  }

  if(!is.null(by)) {
    x_model_avg <- model_avg_df %>%
      select(orig_level, model_avg_lin_nonrescaled_x = model_avg_lin_nonrescaled) %>%
      mutate(orig_level = as.character(orig_level))

    by_model_avg <- compute_model_avg(by, by_betas, current_baseline) %>%
      select(orig_level, model_avg_lin_nonrescaled_by = model_avg_lin_nonrescaled)

    model_avg_df <- tidyr::crossing(orig_level = x_model_avg$orig_level, by = by_model_avg$orig_level) %>%
      dplyr::left_join(x_model_avg, by = "orig_level") %>%
      dplyr::left_join(by_model_avg, by = c("by" = "orig_level")) %>%
      dplyr::mutate(
        model_avg_lin_nonrescaled = model_avg_lin_nonrescaled_x + model_avg_lin_nonrescaled_by - baseline_estimate,
        model_avg_pred_nonrescaled = exp(model_avg_lin_nonrescaled)
      ) %>%
      dplyr::mutate(base_lvl_idx = (by == attr(.env$by, "base_level"))) %>%
      dplyr::group_by(orig_level) %>%
      dplyr::mutate(
        model_avg_lin_rescaled = model_avg_lin_nonrescaled - model_avg_lin_nonrescaled[base_lvl_idx],
        model_avg_pred_rescaled = model_avg_pred_nonrescaled / model_avg_pred_nonrescaled[base_lvl_idx]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        orig_level,
        by,
        model_avg_pred_nonrescaled,
        model_avg_lin_nonrescaled,
        model_avg_pred_rescaled,
        model_avg_lin_rescaled
      )
  }

  model_avg_df %>%
    dplyr::mutate(orig_level = as.character(orig_level)) %>%
    dplyr::mutate(geom_text_label = paste0(round((model_avg_pred_rescaled - 1) * 100), "%"))
}
