# library(dplyr)
#
# if(!'insuranceData' %in% installed.packages()) install.packages('insuranceData')
# data(dataCar, package = "insuranceData")
#
# # define functions to generate policy number and split/save into train/test
# generate_pol_nbr <- function(df, num_digits) {
#   pol_nbr <- vector('character', length = nrow(df))
#
#   min_nbr <- as.numeric(paste0('1', paste0(rep('0', num_digits - 1), collapse = '')))
#   max_nbr <- as.numeric(paste0(rep('9', num_digits), collapse = ''))
#   max_range <- max_nbr - min_nbr
#
#   stopifnot(nrow(df) <= max_range)
#
#   max_increment <- floor(max_range / nrow(df))
#
#   current_nbr <- min_nbr
#   for(i in seq_len(nrow(df))) {
#     pol_nbr[[i]] <- as.character(current_nbr)
#
#     current_nbr <- current_nbr + sample(max_increment, 1)
#   }
#
#   orig_cols <- colnames(df)
#
#   df$pol_nbr <- pol_nbr[sample(nrow(df))]
#
#   df[c('pol_nbr', orig_cols)]
# }
#
# split_and_save <- function(df, name, train_frac = 0.8) {
#   train_size <- round(train_frac * nrow(df))
#   train_rows <- sample(1:nrow(df), size = train_size)
#   test_rows <- setdiff(1:nrow(df), train_rows)
#
#   train_nm <- paste0(name, '_train')
#   assign(train_nm, df[train_rows, ])
#   assign(train_nm, `rownames<-`(get(train_nm), NULL))
#   save(list = train_nm, file = file.path('data', paste0(name, '_train.rda')))
#
#   test_nm <- paste0(name, '_test')
#   assign(test_nm, df[test_rows, ])
#   assign(test_nm, `rownames<-`(get(test_nm), NULL))
#   save(list = test_nm, file = file.path('data', paste0(name, '_test.rda')))
# }
#
# # create target variables
# my_dummy_data <- dataCar %>%
#   generate_pol_nbr(num_digits = 6) %>%
#   dplyr::mutate(premium = 770 * (veh_value / mean(veh_value)) * (1/ (agecat / median(agecat))) * exposure) %>%
#   dplyr::mutate(
#     pol_yr = sample(2000:2004, size = nrow(.), replace = TRUE),
#     veh_value = ggplot2::cut_number(veh_value, n = 5)
#   ) %>%
#   dplyr::mutate(
#     freq = numclaims / exposure,
#     sev = if_else(numclaims > 0, claimcst0 / numclaims, 0),
#     bc = claimcst0 / exposure,
#     lr = if_else(premium > 0, claimcst0 / premium, 0),
#     is_clm = clm
#   )
#
# # create multiple datasets
# select_cols <- c(
#   'pol_nbr',
#   'pol_yr',
#   'exposure',
#   'premium',
#   'gender',
#   'agecat',
#   'area',
#   'veh_body',
#   'veh_age',
#   'veh_value'
# )
#
# predictors <- c(
#   'pol_yr',
#   'gender',
#   'agecat',
#   'area',
#   'veh_body',
#   'veh_age',
#   'veh_value'
# )
#
# target_cols <- list(
#   freq = c('freq'), # TODO - make the poisson GLM work with offset
#   sev = c('numclaims', 'sev'),
#   bc = c('bc'),
#   lr = c('lr'),
#   is_clm = c('is_clm')
# )
#
# for(target in c('freq', 'sev', 'bc', 'lr', 'is_clm')) {
#   df_cols <- c(select_cols, target_cols[[target]])
#
#   d <- my_dummy_data
#   if(target == 'sev') {
#     d <- d %>% dplyr::filter(sev > 0)
#   }
#
#   d[df_cols] %>%
#     dplyr::mutate_at(predictors, as.factor) %>%
#     split_and_save(name = target)
# }
