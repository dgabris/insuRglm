library(dplyr)

# IMPORT and PREPROCESSING
data('sev_train')
data('sev_test')

# SETUP
# TODO - update roxygen to contain setup code for all supported families

# minimal
setup <- setup(
  data_train = train,
  target = 'sev',
  weight = 'numclaims',
  family = 'gamma',
  keep_cols = c('pol_nbr', 'exposure', 'premium')
)

# TODO - rename
# explore_target instead of target_dist
# explore_data instead of data_analyzer
# explore_corr instead of corr_matrix

target_dist(setup)
target_dist(setup, weighted = TRUE)
target_dist(setup, without_zero = TRUE)
target_dist(setup, upper_limit = 5000)
target_dist(setup, lower_limit = 5000)
target_dist(setup, lower_limit = 5000, upper_limit = 6000)

data_analyzer(setup, type = "table")
data_analyzer(setup, type = "graph")
data_analyzer(setup, type = "table", second_dim = "pol_yr")
data_analyzer(setup, type = "graph", second_dim = "pol_yr")

corr_matrix(setup, type = "graph")
corr_matrix(setup, type = "table")

var_importance(setup, direction = "backward")

# MODELING
# TODO - fix error in polynomial variate (argument 'degree')
# e.g. setup %>% factor_add(pol_yr) %>% factor_add(agecat) %>% factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = 1:6, degree = 2)) %>% model_fit()

modeling <- setup %>%
  factor_add(pol_yr) %>%
  factor_add(agecat) %>%
  model_fit() %>%
  model_save('model1') %>%
  factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = 1:6)) %>%
  model_fit() %>%
  model_save('model2') %>%
  factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 1, 2, 3, 4, 5))) %>%
  model_fit() %>%
  model_save('model3') %>%
  factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 3, 4, 5))) %>%
  # model_fit() %>%
  factor_add(veh_value) %>%
  # model_fit() %>%
  factor_remove(veh_value) %>%
  factor_add(veh_age) %>%
  model_fit() %>%
  model_save('model4') %>%
  factor_modify(veh_age = variate(veh_age, type = 'non_prop', mapping = 1:4)) %>%
  # model_fit() %>%
  factor_add(area) %>%
  model_fit() %>%
  model_save('model5') %>%
  factor_modify(area = custom_factor(area, mapping = c(1, 2, 3, 1, 4, 4))) %>%
  model_fit() %>%
  factor_add(veh_value) %>%
  model_fit() %>%
  model_save('model6') %>%
  factor_modify(veh_value = custom_factor(veh_value, mapping = c(1, 2, 3, 3, 3))) %>%
  model_fit() %>%
  model_save('model7') %>%
  factor_modify(veh_value = as_simple_factor(veh_value)) %>%
  factor_modify(veh_value = variate(veh_value, type = 'prop')) %>%
  model_fit() %>%
  model_revert(to = 'model7')

modeling %>%
  model_visualize()

modeling %>%
  model_visualize(factors = 'unfitted')

modeling %>%
  model_betas()

# TODO - allow comparison of saved model to another saved model,
# e.g. what = 'current', with = c('model1', 'model2') or what = 'model3', with = c('model1', 'model2')

modeling %>%
  model_compare(with = c('model7'), type = '1')

# VALIDATION
# TODO - revise the overall logic
# TODO - add option to add your own CV folds (e.g. pick column to validate across)
# TODO - add option to perform temporal validation using time variable (or only select values of time variable)

# TODO - verify if model_lift takes into account 'factor_modify' actions or not
# because lift charts for models before/after simplifications look totally identical

modeling %>%
  model_lift(buckets = 10)

modeling %>%
  model_lift(buckets = 5)

modeling_cv <- modeling %>%
  model_crossval()

modeling_cv %>%
  model_lift(data = 'crossval', buckets = 10)

modeling_cv %>%
  model_lift(data = 'crossval', buckets = 5)

modeling_cv %>%
  model_lift(data = 'crossval', model = 'all', buckets = 10)

# WHAT TO DO WITH TEST SET?
# e.g. score, evaluate, offset?

# SCORING
# TODO - ???

# choose the final model?

# EXPORT
modeling %>%
  model_export('..\\insuRglm_export\\sev_model.xlsx', overwrite = TRUE)




