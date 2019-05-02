data_analyzer <- function(setup, var_list, type = c("graph", "table")) {
  type <- match.arg(type)
  stopifnot(inherits(setup, "setup"))
  stopifnot(typeof(var_list) == "list")

  var_list_member_lengths <- lapply(var_list, length)
  stopifnot(all(var_list_member_lengths %in% c(1, 2)))

  if(type == "graph") {
    for(var_combination in var_list) {
      # something
    }
  } else {
    # something else
  }

  # return result
}
