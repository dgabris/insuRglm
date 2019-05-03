library(dplyr)

data(dataCar, package = "insuranceData")

my_dummy_data <- dataCar %>%
  mutate(veh_value = ggplot2::cut_number(veh_value, n = 5)) %>%
  mutate(
    frequency = numclaims / exposure,
    severity = claimcst0 / numclaims,
    acc_year = sample(2000:2004, size = nrow(.), replace = TRUE)
  ) %>%
  select(-X_OBSTAT_, -clm)

freq_data <- my_dummy_data %>%
  select(-severity, -numclaims, -claimcst0) %>%
  mutate(train_test = if_else(runif(n = nrow(.)) <= 0.8, "train", "test")) %>%
  mutate_at(setdiff(names(.), c("frequency", "exposure")), as.factor) %>%
  tibble::as.tibble()

sev_data <- my_dummy_data %>%
  select(-frequency, -exposure, -claimcst0) %>%
  filter(severity > 0) %>%
  mutate(train_test = if_else(runif(n = nrow(.)) <= 0.8, "train", "test")) %>%
  mutate_at(setdiff(names(.), c("severity", "numclaims")), as.factor) %>%
  tibble::as.tibble()

freq_data %>% filter(train_test == "train") %>% select(-train_test) %>% saveRDS("data/freq_train.rds")
freq_data %>% filter(train_test == "test") %>% select(-train_test) %>% saveRDS("data/freq_test.rds")
sev_data %>% filter(train_test == "train") %>% select(-train_test) %>% saveRDS("data/sev_train.rds")
sev_data %>% filter(train_test == "test") %>% select(-train_test) %>% saveRDS("data/sev_test.rds")
