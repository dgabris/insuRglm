library(dplyr)

# IMPORT and PREPROCESSING
train <- readRDS("data/sev_train.rds")
test <- readRDS("data/sev_test.rds")

# SETUP
setup <- setup(
  data_train = train,
  data_test = test,
  target = "severity",
  weight = "numclaims",
  family = "gamma"
)

target_dist(setup)

data_analyzer(setup, type = "table")
data_analyzer(setup, type = "graph")
data_analyzer(setup, type = "table", second_dim = "acc_year")
data_analyzer(setup, type = "graph", second_dim = "acc_year")

corr_matrix(setup, type = "graph")
corr_matrix(setup, type = "table")

var_importance(setup, direction = "backward")

# MODELING
modeling <- setup %>%
  factor_add(acc_year) %>%
  factor_add(veh_age) %>%
  factor_add(veh_value) %>%
  factor_add(veh_body) %>%
  model_fit() %>%
  model_save("model1") %>%
  factor_modify(
    veh_value = variate(veh_value, type = "prop"),
    veh_body = custom_factor(veh_body, mapping = c(1, 1, 2:6, 7, 7, 8:11))
  ) %>%
  model_fit() %>%
  model_compare(with = "model1")
  #model_revert("model1")

# model_revert

modeling %>%
  model_visualize()

modeling %>%
  model_compare()

# VALIDATION, SCORING, EXPORT
modeling %>%
  model_validate()

final_model <- modeling %>%
  model_choose() %>%
  model_fit()

final_model %>%
  model_score()

final_model %>%
  model_export()




