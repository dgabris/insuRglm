nested_model_test <- function(model_list) {

  stopifnot(typeof(model_list) == "list")
  stopifnot(all(vapply(model_list, function(x) inherits(x, "fitted_model"), logical(1))))

  family <- unique(vapply(model_list, function(x) x$family$family, character(1)))
  stopifnot(length(family) == 1)

  predictor_nums <- vapply(model_list, function(x) length(x$predictors), integer(1))

  combinations_df <- tidyr::crossing(
    a = factor(names(model_list), levels = names(model_list)[order(predictor_nums)]),
    b = factor(names(model_list), levels = names(model_list)[order(predictor_nums)])
  )

  test_results <- vector("character", nrow(combinations_df))

  for(i in seq_len(nrow(combinations_df))) {

    combination <- combinations_df[i, ]

    model_a <- model_list[[combination$a]]
    model_b <- model_list[[combination$b]]
    predictors_a <- model_a$predictors
    predictors_b <- model_b$predictors

    condition_1 <- identical(
      dplyr::semi_join(model_a$betas[1:2], model_b$betas[1:2], by = c("factor", "actual_level")),
      model_a$betas[1:2]
    )

    condition_2 <- identical(
      dplyr::semi_join(model_b$betas[1:2], model_a$betas[1:2], by = c("factor", "actual_level")),
      model_b$betas[1:2]
    )

    is_nested <- xor(condition_1, condition_2)
    is_identical <- condition_1 && condition_2

    if(!is_nested || is_identical) {
      test_results[[i]] <- ""

    } else {

      bigger <- if(length(predictors_a) > length(predictors_b)) model_a else model_b
      smaller <- if(length(predictors_a) > length(predictors_b)) model_b else model_a

      deviance_diff <- smaller$model_stats$deviance - bigger$model_stats$deviance
      df_diff <- smaller$model_stats$df.residual - bigger$model_stats$df.residual

      scale <- bigger$model_stats$dispersion
      df_scale <- if(scale == 1) Inf else bigger$model_stats$df.residual

      if(family == "poisson") {

        test <- "Chi"
        test_stat <- (deviance_diff / scale) * sign(df_diff)
        p_value <- pchisq(test_stat, df_diff, lower.tail = FALSE)

      } else if(family %in% c("Gamma", "Tweedie")) {

        test <- "F"
        test_stat <- (deviance_diff / df_diff) / scale
        p_value <- pf(test_stat, df_diff, df_scale, lower.tail = FALSE)

      }

      test_results[[i]] <- paste0("Pr(>", test, ") = ", round(p_value, 5))

    }

  }

  combinations_df <- combinations_df %>%
    dplyr::mutate(test_result = test_results) %>%
    tidyr::spread(key = b, value = test_result) %>%
    dplyr::rename(model = a)

  n_row <- nrow(combinations_df)
  combinations_matrix <- as.matrix(combinations_df)

  upper_index <- upper.tri(matrix(data = "anything", nrow = n_row, ncol = n_row), diag = TRUE)
  combinations_matrix[, -1][upper_index] <- ""

  tibble::as_tibble(combinations_matrix)

}
