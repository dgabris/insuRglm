compute_model_avg <- function(x, x_betas, current_baseline, by = NULL, by_betas = NULL, data_attrs = NULL) {

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

    # TODO - separate this branch into standalone function?
    stopifnot(!is.null(data_attrs))

    main_effects <- attr(x, "main_effects")
    components <- attr(x, "components")
    is_triple_interaction <- length(main_effects) == 3

    base_df <- tibble::tibble(orig_level = levels(x)) %>%
      tidyr::separate(orig_level, into = main_effects, sep = "__") %>%
      dplyr::mutate_at(main_effects, function(x) stringr::str_replace(x, "^(,)(.+)(,)$", "\\2")) %>%
      mutate(estimate_sum = baseline_estimate)

    for(main_effect in main_effects) {
      effect_betas <- x_betas %>%
        dplyr::filter(factor == main_effect) %>%
        dplyr::select(actual_level, estimate)

      base_df <- base_df %>%
        dplyr::left_join(effect_betas, by = setNames("actual_level", main_effect)) %>%
        dplyr::mutate(
          estimate = coalesce(estimate, 0),
          estimate_sum = estimate_sum + estimate
        ) %>%
        dplyr::select(-estimate)
    }

    for(interaction_effect in components) {

      is_custom_factor <- data_attrs[[interaction_effect]]$class[[1]] == "custom_factor"
      is_variate <- data_attrs[[interaction_effect]]$class[[1]] == "variate"
      is_offset <- nrow(dplyr::filter(x_betas, factor == interaction_effect)) == 0

      if(is_triple_interaction) {
        parent_var <- data_attrs[[interaction_effect]]$parent_var
        sep_into <- data_attrs[[parent_var]]$main_effects
      } else {
        sep_into <- main_effects
      }

      if(is_custom_factor) {
        mapping <- data_attrs[[interaction_effect]]$mapping

        effect_betas <- x_betas %>%
          dplyr::filter(factor == interaction_effect) %>%
          dplyr::select(actual_level, estimate)

        effect_betas <- tibble::tibble(
          orig_level = names(mapping),
          actual_level = as.character(mapping)
        ) %>%
          dplyr::left_join(effect_betas, by = "actual_level") %>%
          tidyr::separate(orig_level, into = sep_into, sep = "__") %>%
          dplyr::mutate_at(sep_into, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")) %>%
          dplyr::select(-actual_level) %>%
          dplyr::filter(!is.na(estimate))

      } else if(is_variate) {
        mapping <- data_attrs[[interaction_effect]]$mapping
        base_level <- data_attrs[[interaction_effect]]$base_level

        baseline_x_vals <- mapping %>%
          dplyr::filter(as.character(orig_level) == base_level) %>%
          dplyr::select(dplyr::contains("orthogonal_degree_"))

        stopifnot(nrow(baseline_x_vals) == 1)
        baseline_x_vals <- unlist(baseline_x_vals, use.names = FALSE)

        effect_betas <- x_betas %>%
          dplyr::filter(factor == interaction_effect)

        # baseline already contains adjustment from this variate, need to subtract it
        adjustment <- (-1) * sum(effect_betas$estimate * baseline_x_vals)

        estimate_vector <- rep(adjustment, nrow(mapping))

        for(i in seq_along(nrow(effect_betas))) {
          tmp <- mapping[[paste0("orthogonal_degree_", i)]] * effect_betas$estimate[[i]]
          estimate_vector <- estimate_vector + tmp
        }

        effect_betas <- tibble::tibble(
          actual_level = mapping$orig_level,
          estimate = estimate_vector
        ) %>%
          tidyr::separate(actual_level, into = sep_into, sep = "__") %>%
          dplyr::mutate_at(sep_into, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2"))

      } else if(is_offset) {
        effect_betas <- tibble::tibble(
          actual_level = names(data_attrs[[interaction_effect]]$mapping),
          estimate = unname(data_attrs[[interaction_effect]]$mapping)
        ) %>%
          tidyr::separate(actual_level, into = sep_into, sep = "__") %>%
          dplyr::mutate_at(sep_into, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2"))

      } else {
        # simple factor
        effect_betas <- x_betas %>%
          dplyr::filter(factor == interaction_effect) %>%
          tidyr::separate(actual_level, into = sep_into, sep = "__") %>%
          dplyr::mutate_at(sep_into, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")) %>%
          dplyr::select(-factor, -std_error, -std_error_pct)

        # TODO - simplify this into one
        # if(is_triple_interaction) {
        #   effect_betas <- x_betas %>%
        #     dplyr::filter(factor == interaction_effect) %>%
        #     tidyr::separate(factor, into = c("primary_var", "secondary_var"), sep = "__X__") %>%
        #     dplyr::mutate(secondary_var = stringr::str_replace(secondary_var, "_[^_]+$", "")) %>%
        #     tidyr::separate(actual_level, into = c(.$primary_var[[1]], .$secondary_var[[1]]), sep = "__") %>%
        #     dplyr::mutate_at(
        #       c(.$primary_var[[1]], .$secondary_var[[1]]), function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")
        #     ) %>%
        #     dplyr::select(-primary_var, -secondary_var, -std_error, -std_error_pct)
        #
        # } else {
        #   effect_betas <- x_betas %>%
        #     dplyr::filter(factor == interaction_effect) %>%
        #     tidyr::separate(actual_level, into = main_effects, sep = "__") %>%
        #     dplyr::mutate_at(main_effects, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")) %>%
        #     dplyr::select(-factor, -std_error, -std_error_pct)
        # }

      }

      join_keys <- base::intersect(colnames(base_df), colnames(effect_betas))

      base_df <- base_df %>%
        dplyr::left_join(effect_betas, by = join_keys) %>%
        dplyr::mutate(
          estimate = coalesce(estimate, 0),
          estimate_sum = estimate_sum + estimate
        ) %>%
        dplyr::select(-estimate)
    }

    if(is_triple_interaction) {
      # add the triple interaction itself
      effect_betas <- x_betas %>%
        dplyr::filter(factor == attr(x, "var_nm")) %>%
        tidyr::separate(actual_level, into = main_effects, sep = "__") %>%
        dplyr::mutate_at(main_effects, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")) %>%
        dplyr::select(-factor, -std_error, -std_error_pct)

      join_keys <- main_effects

      base_df <- base_df %>%
        dplyr::left_join(effect_betas, by = join_keys) %>%
        dplyr::mutate(
          estimate = coalesce(estimate, 0),
          estimate_sum = estimate_sum + estimate
        ) %>%
        dplyr::select(-estimate)
    }

    model_avg_df <- base_df %>%
      dplyr::mutate(
        model_avg_pred_nonrescaled = exp(estimate_sum),
        model_avg_lin_nonrescaled = estimate_sum
      ) %>%
      dplyr::select(-estimate_sum)

    scaling_var <- main_effects[[2]]
    scaling_base_level <- data_attrs[[scaling_var]]$base_level

    scaling_df <- model_avg_df %>%
      dplyr::filter(.data[[scaling_var]] == scaling_base_level) %>%
      dplyr::rename(scaler_pred = model_avg_pred_nonrescaled, scaler_lin = model_avg_lin_nonrescaled) %>%
      dplyr::select(-!!rlang::sym(scaling_var))

    model_avg_df <- model_avg_df %>%
      dplyr::left_join(scaling_df, by = setdiff(main_effects, scaling_var)) %>%
      dplyr::mutate(
        model_avg_pred_rescaled = model_avg_pred_nonrescaled / scaler_pred,
        model_avg_lin_rescaled = model_avg_lin_nonrescaled / scaler_lin
      ) %>%
      dplyr::select(-scaler_pred, -scaler_lin)

    # model_avg_df <- base_df %>%
    #   dplyr::mutate(
    #     model_avg_pred_nonrescaled = exp(estimate_sum),
    #     model_avg_lin_nonrescaled = estimate_sum,
    #     model_avg_pred_rescaled = exp(estimate_sum - baseline_estimate),
    #     model_avg_lin_rescaled = estimate_sum - baseline_estimate
    #   ) %>%
    #   dplyr::select(-estimate_sum)

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

  if(!inherits(x, "interaction")) {
    model_avg_df <- model_avg_df %>%
      dplyr::mutate(orig_level = as.character(orig_level)) %>%
      dplyr::mutate(geom_text_label = paste0(round((model_avg_pred_rescaled - 1) * 100), "%"))
  }

  model_avg_df
}
