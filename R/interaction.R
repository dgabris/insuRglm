#' Create variable interaction
#'
#' @param x Unquoted symbol. One of the variables for interaction.
#' @param y Unquoted symbol. One of the variables for interaction.
#' @param z Unquoted symbol. One of the variables for interaction.
#'
#' @return Vector of newly created interaction
#' @export
#'
#' @examples
#' require(dplyr) # for the pipe operator
#' data('bc_train')
#'
#' setup <- setup(
#'   data_train = bc_train,
#'   target = 'bc',
#'   weight = 'exposure',
#'   family = 'tweedie',
#'   tweedie_p = 1.5,
#'   keep_cols = c('pol_nbr', 'premium')
#' )
#'
#' modeling <- setup %>%
#'   factor_add(pol_yr) %>%
#'   factor_add(agecat) %>%
#'   factor_add(gender) %>%
#'   model_fit() %>%
#'   model_save("model1") %>%
#'   factor_modify(agecat_X_gender = interaction(agecat, gender)) %>%
#'   factor_add(agecat_X_gender) %>%
#'   model_fit() %>%
#'   model_save("model2") %>%
#'   # modification of 'agecat' will simplify the base level of interaction (agecat_X_gender_F)
#'   factor_modify(agecat = variate(agecat, type = "non_prop", mapping = 1:6)) %>%
#'   # all other non-base levels need to be simplified separately
#'   factor_modify(agecat_X_gender_M = variate(agecat_X_gender_M, type = "non_prop", mapping = 1:6, degree = 2)) %>%
#'   model_fit()
#'

interaction <- function(x, y, z = NULL) {

  if(length(levels(x)) > length(levels(y))) {
    primary_var <- x
    secondary_var <- y
  } else {
    primary_var <- y
    secondary_var <- x
  }

  var_list <- list(x = primary_var, y = secondary_var)
  if(!is.null(z)) var_list$z <- z

  stopifnot(all(vapply(var_list, function(x) inherits(x, "simple_factor"), logical(1))))

  base_levels <- vapply(var_list, function(x) attr(x, "base_level"), character(1))

  replacements <- c(
    "\\|" = "\\\\|", "\\(" = "\\\\(", "\\)" = "\\\\)",
    "\\[" = "\\\\[", "\\]" = "\\\\]", "\\." = "\\\\."
  )

  base_lvl_regex <- base_levels %>%
    stringr::str_replace_all(replacements) %>%
    paste0("(__)?(,", ., ",)(__)?") %>%
    paste(collapse = "|")

  new_base_level <- paste(paste0(",", base_levels, ","), collapse = "__")

  var_list <- lapply(var_list, function(x) {levels(x) <- paste0(",", levels(x), ","); x})
  result <- base::interaction(var_list, sep = "__")

  orig_levels <- levels(result)
  new_levels <- dplyr::if_else(stringr::str_detect(orig_levels, base_lvl_regex), new_base_level, orig_levels)

  mapping <- setNames(new_levels, orig_levels)

  attr(result, "orig_levels") <- orig_levels
  attr(result, "mapping") <- mapping
  attr(result, "base_level") <- new_base_level

  attr(result, "main_effects") <- unname(vapply(var_list, function(x) attr(x, "var_nm"), character(1)))
  class(result) <- c("interaction", class(as_simple_factor(x)))

  result
}


# interaction <- function(vars_list) {
#
#   stopifnot(typeof(vars_list) == "list")
#
#   base_levels <- vapply(vars_list, function(x) attr(x, "base_level"), character(1))
#
#   base_levels_for_regex <- base_levels %>%
#     stringr::str_replace_all("\\|", "\\\\|") %>%
#     stringr::str_replace_all("\\(", "\\\\(") %>%
#     stringr::str_replace_all("\\)", "\\\\)") %>%
#     stringr::str_replace_all("\\[", "\\\\[") %>%
#     stringr::str_replace_all("\\]", "\\\\]") %>%
#     stringr::str_replace_all("\\.", "\\\\.")
#
#   base_levels_regex <- paste(base_levels_for_regex, collapse = "|")
#
#   new_base_level <- paste(base_levels, collapse = ".")
#
#   x <- interaction(vars_list)
#
#   orig_levels <- levels(x)
#   new_levels <- dplyr::if_else(stringr::str_detect(orig_levels, base_levels_regex), new_base_level, orig_levels)
#
#   mapping <- setNames(new_levels, orig_levels)
#
#   attr(x, "orig_levels") <- orig_levels
#   attr(x, "mapping") <- mapping
#   attr(x, "base_level") <- new_base_level
#
#   class(x) <- c("interaction", class(x))
#
#   x
# }
