#' Export the current insuRglm model
#'
#' Exports the current model into xlsx file. The excel spreadsheet will contain the charts, as well as
#' relativities and weights for each predictor included in the current (last) model.
#'
#' @param setup Setup object. Created at the start of the workflow. Usually piped in from previous step.
#' @param xlsx_file Character scalar. Path and name of the file to store the exported model.
#' @param overwrite Boolean scalar. Whether to overwrite an existing file.
#'
#' @return NULL, this function has only side effects.
#' @export
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
#'   model_fit()
#'
#' modeling %>%
#'   model_export('export_test.xlsx', overwrite = TRUE)
#'

model_export <- function(setup, xlsx_file, overwrite = FALSE) {
  if(!inherits(setup, 'setup')) stop('Setup object is not correct')
  if(!inherits(setup, 'modeling')) stop("No model is fitted. Please run 'model_fit' first")
  if(!(is.character(xlsx_file) && length(xlsx_file) == 1)) stop("'xlsx_file' must be a character scalar")

  main_sheet_name <- "All"
  image_width_cm <- 20
  image_height_cm <- 14.5

  if(!stringr::str_detect(xlsx_file, ".xlsx$")) {
    xlsx_file <- paste0(xlsx_file, ".xlsx")
  }

  # predictor_names <- setup$current_model$predictors
  predictor_names <- names(setup$current_model$relativities)[-1]

  base_df <- setup$current_model$betas %>%
    dplyr::filter(factor == "(Intercept)") %>%
    dplyr::mutate(base_value = exp(estimate)) %>%
    dplyr::select(base_value)

  relativities <- setup$current_model$relativities
  charts <- model_visualize(setup)[seq_len(length(relativities) - 1)]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = main_sheet_name)
  openxlsx::writeDataTable(wb, sheet = main_sheet_name, x = base_df, startRow = 2, startCol = 2)

  main_sheet_row <- 2 + 2 + 1
  for(i in seq_along(predictor_names)) {
    predictor_name <- stringr::str_replace_all(predictor_names[[i]], "\\*", "-")
    openxlsx::addWorksheet(wb, sheetName = predictor_name)

    relativity_df <- tibble::as_tibble(relativities[[i + 1]])
    nrows_table <- nrow(relativity_df)
    ncols_table <- ncol(relativity_df)

    openxlsx::writeDataTable(
      wb, sheet = main_sheet_name, x = relativity_df, startRow = main_sheet_row, startCol = 2
    )

    main_sheet_row <- main_sheet_row + nrows_table + 1 + 1

    openxlsx::writeDataTable(
      wb, sheet = predictor_name, x = relativity_df, startRow = 2, startCol = 2
    )

    image_file <- paste0(tempfile(), ".png")
    png(image_file, width = image_width_cm, height = image_height_cm, units = "cm", res = 300)
    print(charts[[i]])
    dev.off()
    openxlsx::insertImage(
      wb, sheet = predictor_name, file = image_file, units = "cm", width = image_width_cm, height = image_height_cm,
      startRow = 2, startCol = 2 + 1 + ncols_table
    )
  }

  suppressMessages(openxlsx::saveWorkbook(wb, file = xlsx_file, overwrite = overwrite))
}
