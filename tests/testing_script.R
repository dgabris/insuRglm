train <- readRDS("data/sev_train.rds")
test <- readRDS("data/sev_test.rds")

setup <- setup(
  data_train = train,
  data_test = test,
  target = "severity",
  weight = "numclaims",
  time_var = "acc_year",
  family = "gamma"
)

target_dist <- target_dist(setup)
target_dist$train
target_dist$test

corr_matrix(setup, type = "graph")

var_importance(setup)

modeling <- setup %>%
  factor_add(acc_year) %>%
  factor_add(veh_age) %>%
  factor_add(gender) %>%
  factor_add(agecat) %>%
  factor_add(veh_value) %>%
  factor_remove(veh_value) %>%
  factor_modify(
    veh_value = variate(veh_value, type = "non_prop"),
    veh_body = custom_factor(veh_body, mapping = c(1, 1, 2:6, 7, 7, 8:11))
  ) %>%
  factor_add(veh_value) %>%
  factor_add(veh_body) %>%
  factor_remove(whatever) %>%
  factor_remove(veh_value) %>%
  fit()

# model_visualize
# model_save
# model_revert
# model_export


