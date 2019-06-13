model_leverage <- function(setup, type = c("1", "2")) {
  stopifnot(inherits(setup, "setup"))
  stopifnot(inherits(setup, "modeling"))
  type <- match.arg(type)

  model_list <- list()

  for(model_nm in names(setup$ref_models)) {
    model_list[[model_nm]] <- setup$ref_models[[model_nm]]
  }

  model_list$current_model <- setup$current_model

  counter = 0
  for(model_nm in names(model_list)) {
    counter = counter + 1

    leverage_plots <- model_list[[model_nm]]$leverage_plots
    leverage_plots[[1]][[1]][[10]][[2]][[2]] <- paste0("Cook's Distance", " (", model_nm, ")")
    leverage_plots[[2]][[1]][[22]][[2]][[2]] <- paste0("Residuals vs Leverage", " (", model_nm, ")")

    print(leverage_plots[[as.integer(type)]])

    if(counter < length(model_list)) plot.new()
  }

}
