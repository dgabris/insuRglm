beta_triangles <- function(betas, glm) {
  stopifnot(inherits(betas, "data.frame"))
  stopifnot(inherits(glm, "glm"))

  vcov_estimate_matrix <- vcov(glm)
  vcov_estimate_cols <- colnames(vcov_estimate_matrix)
  vcov_estimate_rows <- rownames(vcov_estimate_matrix)

  predictors <- setdiff(unique(betas$factor), "(Intercept)")
  result <- list()

  for(predictor in predictors) {
    predictor_sym <- rlang::sym(predictor)

    filtered_betas <- dplyr::filter(betas, factor == predictor)
    names <- filtered_betas$factor
    levels <- filtered_betas$actual_level

    is_variate <- length(names) == 1 && stringr::str_detect(levels, "degree.")

    if(!is_variate) {
      predictor_labels <- paste0("`", names, " `", levels)
      estimates <- setNames(filtered_betas$estimate, predictor_labels)

      row_index <- match(predictor_labels, vcov_estimate_rows)
      col_index <- match(predictor_labels, vcov_estimate_cols)

      predictor_vcov_matrix <- vcov_estimate_matrix[row_index, col_index, drop = FALSE]

      combinations_df <- tidyr::crossing(
        a = factor(predictor_labels, levels = predictor_labels),
        b = factor(predictor_labels, levels = predictor_labels)
      )

      std_error_vector <- vector("numeric", nrow(combinations_df))

      for(i in seq_len(nrow(combinations_df))) {

        combination <- combinations_df[i, ]

        estimate_diff <- estimates[[combination$a]] - estimates[[combination$b]]

        var_a <- predictor_vcov_matrix[[combination$a, combination$a]]
        var_b <- predictor_vcov_matrix[[combination$b, combination$b]]
        cov_ab <- predictor_vcov_matrix[[combination$a, combination$b]]

        var_diff <- (var_a + var_b) - (2 * cov_ab)

        std_error_vector[[i]] <- sqrt(var_diff) / abs(estimate_diff)
      }

      combinations_df <- combinations_df %>%
        dplyr::mutate(std_error_pct = paste0(round(std_error_vector * 100), "%")) %>%
        dplyr::mutate_at(c("a", "b"), stringr::str_replace_all, "`", "") %>%
        dplyr::mutate_at(c("a", "b"), stringr::str_replace, predictor, "") %>%
        dplyr::mutate_at(c("a", "b"), stringr::str_replace, "^ ", "") %>%
        dplyr::mutate_at(c("a", "b"), function(x) factor(x, levels = unique(x))) %>%
        tidyr::spread(key = b, value = std_error_pct) %>%
        dplyr::rename(!!predictor_sym := a)

      n_row <- nrow(combinations_df)
      combinations_matrix <- as.matrix(combinations_df)

      upper_index <- upper.tri(matrix(data = "anything", nrow = n_row, ncol = n_row), diag = TRUE)
      combinations_matrix[, -1][upper_index] <- ""

      combinations_df <- tibble::as_tibble(combinations_matrix)

    } else{
      combinations_df <- tibble::tibble(!!predictor_sym := "variate")
    }

    result[[predictor]] <- combinations_df

  }

  result

}
