#' Create or modify factor in the modeling dataset
#'
#' Modifies one or more factors in the modeling dataset. Predictors in the model formula can also be modified directly.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param ... Expression. One or more named expressions (similar to dplyr::mutate).
#'
#' @return Setup object with updated attributes.
#' @export
#'
#' @seealso \code{\link{custom_factor}}, \code{\link{variate}}, \code{\link{as_simple_factor}},
#' \code{\link{offset}}
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
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat) %>%
#'   model_fit() %>%
#'   model_save('model1') %>%
#'   factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) %>%
#'   model_fit()
#'

factor_modify <- function(setup, ...) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')

  dots <- rlang::enexprs(...)
  vars <- names(dots)

  if(any(vars == '')) stop('Please provide a named expression')

  # disable for now
  # if(!all(vars %in% setup$simple_factors)) stop('Please use the original predictor name as the name of argument')
  # enable for now

  get_fun_called <- function(dot) {
    fun_called <- as.character(dot[[1]])
    if(length(fun_called) == 3) {
      fun_called[[3]]
    } else {
      fun_called[[1]]
    }
  }

  funs_called <- vapply(dots, get_fun_called, character(1))
  funs_supported <- c("as_simple_factor", "custom_factor", "variate", "interaction", "offset_term")
  modify_funs <- setdiff(funs_supported, "interaction")
  target_args <- vars
  main_args <- vapply(dots, function(dot) as.character(dot[[2]]), character(1))

  for(i in seq_along(funs_called)) {
    fun_called <- funs_called[[i]]
    target_arg <- target_args[[i]]
    main_arg <- main_args[[i]]
    call_string <- rlang::expr_deparse(dots[[i]])

    if(!fun_called %in% funs_supported) {
      stop(paste0("'", fun_called, "' is not a supported function within 'factor_modify'. Currently supported ",
                  "functions are ", paste0(paste0("'", funs_supported, "'"), collapse = ", "), "."))
    }

    if(fun_called %in% modify_funs) {
      if(all(c(target_arg, main_arg) %in% setup$simple_factors)) {
        if(target_arg != main_arg) {
          stop(paste0("'", target_arg, " = ", call_string, "' is not a supported operation within 'factor_modify'. You can't ",
                      " modify '", target_arg, "' using '", main_arg, "'."))
        }
      }
    }
  }

  setup$data_train <- setup$data_train %>%
    dplyr::mutate(!!!dots)

  if(!is.null(setup$data_test)) {
    setup$data_test <- setup$data_test %>%
      dplyr::mutate(!!!dots)
  }

  new_vars <- vars[!vars %in% setup$simple_factors]

  if(length(new_vars) > 0) {
    train <- setup$data_train
    target <- train[[setup$target]]
    weight <- train[[setup$weight]]

    for(var in new_vars) {
      is_interaction <- inherits(train[[var]], "interaction")
      is_component <- inherits(train[[var]], "component")

      if(is_interaction) {
        setup$obs_avg_tables[[var]] <- compute_obs_avg(train[[var]], target, weight)

      } else if(!(is_interaction || is_component)) {
        # modified variable
        setup$obs_avg_tables[[var]] <- setup$obs_avg_tables[[orig_var]]
        orig_var_idx <- which(setup$simple_factors == orig_var)

        setup$simple_factors <- c(
          setup$simple_factors[1:orig_var_idx],
          var,
          setup$simple_factors[(orig_var_idx + 1):length(setup$simple_factors)]
        )
      }
    }
  }

  if(any(vars %in% setup$current_model$predictors)) {
    class(setup$current_model) <- "unfitted_model"
  }

  is_interaction <- vapply(vars, function(var_nm) inherits(setup$data_train[[var_nm]], "interaction"), logical(1))

  if(any(is_interaction)) {
    interaction_vars <- vars[is_interaction]

    for(var in interaction_vars) {
      main_effects <- attr(setup$data_train[[var]], "main_effects")

      if(length(main_effects) == 2) {
        primary_var <- setup$data_train[[main_effects[[1]]]]
        secondary_var <- setup$data_train[[main_effects[[2]]]]

        non_base_levels <- setdiff(attr(secondary_var, "orig_levels"), attr(secondary_var, "base_level"))

        component_names <- c()
        for(non_base_lvl in non_base_levels) {
          component_var_nm <- paste0(var, "_", non_base_lvl)
          component_names <- c(component_names, component_var_nm)

          interaction_copy <- setup$data_train[[var]]
          base_level <- attr(interaction_copy, "base_level")

          base_level_regex <- base_level %>%
            stringr::str_split(pattern = "__", simplify = TRUE) %>%
            as.vector() %>%
            paste0("(__)?(", ., ")(__)?") %>%
            paste0(collapse = "|")

          orig_levels <- attr(interaction_copy, "orig_levels")
          replace_levels1 <- orig_levels[stringr::str_detect(orig_levels, base_level_regex)]
          replace_levels2 <- orig_levels[!stringr::str_detect(orig_levels, paste0(",", non_base_lvl, ","))]
          replace_levels <- unique(c(replace_levels1, replace_levels2))

          argument_list <- list(replace_levels)
          names(argument_list) <- base_level
          argument_list <- c(
            .f = list(interaction_copy),
            argument_list
          )

          interaction_copy <- do.call(forcats::fct_collapse, argument_list)

          orig_levels <- levels(interaction_copy)

          # need to restore primary variable level order
          new_level_order <- tibble::tibble(orig_level = orig_levels) %>%
            tidyr::separate(orig_level, into = main_effects, sep = "__") %>%
            dplyr::mutate_at(main_effects, function(x) stringr::str_replace(x, "(,)(.+)(,)", "\\2")) %>%
            dplyr::pull(main_effects[[1]]) %>%
            match(levels(primary_var))

          orig_levels <- orig_levels[order(new_level_order)]

          tmp_attr <- attributes(interaction_copy)
          tmp_attr$levels <- orig_levels

          interaction_copy <- factor(interaction_copy, levels = orig_levels)
          attributes(interaction_copy) <-  tmp_attr

          attr(interaction_copy, "orig_levels") <- orig_levels
          component_var <- as_simple_factor(interaction_copy)
          class(component_var) <- c(class(component_var)[[1]], "component", class(component_var)[-1])
          attr(component_var, "parent_var") <- var
          setup$data_train[[component_var_nm]] <- component_var

          if(!is.null(setup$data_test)) {
            interaction_copy <- setup$data_test[[var]]

            argument_list$.f <- interaction_copy

            interaction_copy <- do.call(forcats::fct_collapse, argument_list)
            interaction_copy <- factor(interaction_copy, levels = orig_levels)
            attributes(interaction_copy) <-  tmp_attr
            attr(interaction_copy, "orig_levels") <- orig_levels
            component_var <- as_simple_factor(interaction_copy)
            class(component_var) <- c(class(component_var)[[1]], "component", class(component_var)[-1])
            attr(component_var, "parent_var") <- var
            setup$data_test[[component_var_nm]] <- component_var
          }

          message(paste0("You can now simplify the interaction of '", attr(primary_var, "var_nm"),
                         "' with non-base level '", non_base_lvl, "' of '", attr(secondary_var, "var_nm"),
                         "' through the newly created factor '", component_var_nm, "'."))
        }

      } else if(length(main_effects) == 3) {
        n_levels <- vapply(main_effects, function(main_eff) {
          length(attr(setup$data_train[[main_eff]], "orig_levels"))
          }, integer(1)
        )

        combinations <- combn(main_effects, 2, simplify = FALSE) %>%
          lapply(function(x) x[order(n_levels[x], decreasing = TRUE)])

        interaction_names <- vapply(combinations, function(x) paste0(x, collapse = "__X__"), character(1))
        # component_names <- vapply(combinations, function(x) paste0(x, collapse = "__X__"), character(1))

        component_names <- c()
        for(i in seq_along(combinations)) {
          new_var_char <- interaction_names[[i]]
          new_var_sym <- rlang::sym(new_var_char)

          suppressMessages({setup <- setup %>%
            factor_modify(
              !!new_var_sym := interaction(
                x = !!rlang::sym(combinations[[i]][[1]]),
                y = !!rlang::sym(combinations[[i]][[2]])
              )
            )
          })

          component_names <- c(component_names, attr(setup$data_train[[new_var_char]], "components"))

          message(paste0("The interaction '", interaction_names[[i]], "' was created in the dataset."))
        }

        attr(setup$data_train[[var]], "interactions") <- interaction_names
        if(!is.null(setup$data_test)) {
          attr(setup$data_test[[var]], "interactions") <- interaction_names
        }
      }

      attr(setup$data_train[[var]], "components") <- component_names
      attr(setup$data_train[[var]], "var_nm") <- var
      if(!is.null(setup$data_test)) {
        attr(setup$data_test[[var]], "components") <- component_names
        attr(setup$data_test[[var]], "var_nm") <- var
      }

    }
  }

  setup
}
