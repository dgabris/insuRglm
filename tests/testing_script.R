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

setup %>%
  add(acc_year) %>%
  add(veh_age) %>%
  add(gender) %>%
  add(agecat) %>%
  add(veh_value) %>%
  remove(veh_value) %>%
  modify(
    veh_value = variate(veh_value, type = "non_prop"),
    veh_body = custom_factor(veh_body, mapping = c(1, 1, 2:6, 7, 7, 8:11))
  ) %>%
  add(veh_value) %>%
  add(veh_body) %>%
  remove(whatever) %>%
  fit()


