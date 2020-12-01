context("Setup - Input data")

test_that("input datasets must be of class data.frame", {

  args <- list(
    data_train = as.list(sev_train),
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "'data_train' must be of class 'data.frame'")

  args$data_test <- as.list(sev_test)

  expect_error(do.call(setup, args), "'data_train' and 'data_test' must be of class 'data.frame'")

})

test_that("input datasets must have consistent schema", {

  sev_test_missing_column <- sev_test %>%
    dplyr::select(-agecat)

  args <- list(
    data_train = sev_train,
    data_test = sev_test_missing_column,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  err_msg <- paste0(
    "'data_train' and 'data_test' don't have the same columns or their types. ",
    "Please check this using 'colnames' and 'class' functions."
  )

  expect_error(do.call(setup, args), err_msg)

  sev_test_different_type <- sev_test %>%
    dplyr::mutate_at("gender", as.character)

  args$data_test <- sev_test_different_type

  expect_error(do.call(setup, args), err_msg)
})

test_that("data_test can't contain extra values that are not in data_train", {

  sev_train <- sev_train %>%
    dplyr::mutate(test_column = as.character(sample(c('a', 'b', 'c'), size = nrow(.), replace = TRUE)))

  sev_test <- sev_test %>%
    dplyr::mutate(test_column = as.character(sample(c('a', 'b', 'c', 'd'), size = nrow(.), replace = TRUE)))

  args <- list(
    data_train = sev_train,
    data_test = sev_test,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "test_column has these values in test set, that are not present in train: d")

})

context("Setup - Target")

test_that("target argument must be a scalar", {

  args <- list(
    data_train = sev_train,
    target = c('sev', 'sev'),
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "'target' must be a character scalar"))

})

test_that("target variable must be present in the datasets", {

  args <- list(
    data_train = sev_train,
    target = 'imaginary_target',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "Target variable not in the dataset"))

})


context("Setup - Weight")

test_that("setup works even without weight argument", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    # weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  no_print(setup <- do.call(setup, args))

  expect_equal(setup$weight, "_weight")
  expect_equal(setup$data_train[[setup$weight]], rep(1, nrow(setup$data_train)))

})

test_that("weight argument must be a scalar", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = c('numclaims', 'numclaims'),
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "'weight' must be a character scalar"))

})

test_that("weight variable must be present in the datasets", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'imaginary_weight',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "Weight variable not in the dataset"))

})


context("Setup - Offset")

test_that("offset argument must be a scalar", {

  args <- list(
    data_train = freq_train,
    target = 'numclaims',
    offset = c('exposure', 'exposure'),
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "'offset' must be a character scalar"))

})

test_that("offset variable must be present in the datasets", {

  args <- list(
    data_train = freq_train,
    target = 'numclaims',
    offset = 'imaginary_offset',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args, "Offset variable not in the dataset"))

})


context("Setup - Keep columns")

test_that("keep_cols argument must be a character vector", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = as.factor(c('pol_nbr', 'exposure', 'premium')),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "'keep_cols' must be a character vector")

})

test_that("keep_cols variables must be present in the datasets", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'imaginary_column'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "Some of the 'keep_cols' are not in the dataset")

})


context("Setup - Simple factors")

test_that("setup works even without simple_factors argument", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  no_print(setup <- do.call(setup, args))

  correct_result <- c(
    "pol_yr",
    "gender",
    "agecat",
    "area",
    "veh_body",
    "veh_age",
    "veh_value"
  )

  expect_equal(setup$simple_factors, correct_result)

})

test_that("simple_factors must be a character vector", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    simple_factors = as.factor(c("pol_yr", "agecat", "veh_age")),
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(
    do.call(setup, args),
    "'simple_factors' must be a character vector of names of potential predictor columns in the dataset"
  )

})

test_that("simple_factors variables must be present in the datasets", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    simple_factors = c("pol_yr", "agecat", "imaginary_column"),
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "Some of the 'simple_factors' are not in the dataset")

})

test_that("non-factor simple_factors get coerced to factor class", {

  testing_factors <- c("pol_yr", "agecat", "veh_age")

  sev_train <- sev_train %>%
    dplyr::mutate_at(testing_factors, as.character)

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    simple_factors = testing_factors,
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  no_print(setup <- do.call(setup, args))

  actual_classes <- vapply(setup$data_train[testing_factors], class, character(2))
  correct_classes <- magrittr::set_colnames(rbind(rep("simple_factor", 3), rep("factor", 3)), testing_factors)

  expect_equal(actual_classes, correct_classes)
})

test_that("non-factor class simple_factors can't have more than 255 unique values", {

  sev_train <- sev_train %>%
    dplyr::mutate(test_column = as.character(sample(1:1000, size = nrow(.), replace = TRUE)))

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  err_msg <- paste0(
    "Some of the 'simple_factors' columns have more than 255 unique values: ",
    "test_column",
    ". If these are not predictors, but columns you want to keep, please include them in 'keep_cols' argument"
  )

  expect_error(do.call(setup, args), err_msg)

})

test_that("factor class simple_factors can't have more than 255 unique values", {

  sev_train <- sev_train %>%
    dplyr::mutate(test_column = as.factor(sample(1:1000, size = nrow(.), replace = TRUE)))

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  err_msg <- paste0(
    "Some of the 'simple_factors' columns have more than 255 unique values: ",
    "test_column",
    ". If these are not predictors, but columns you want to keep, please include them in 'keep_cols' argument"
  )

  expect_error(do.call(setup, args), err_msg)

})

test_that("all simple_factors are of factor class after setup was run", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  no_print(setup1 <- do.call(setup, args))

  actual_result <- vapply(setup1$data_train[setup1$simple_factors], class, character(2))
  actual_result <- unique(as.vector(actual_result))

  correct_result <- c("simple_factor", "factor")

  expect_equal(actual_result, correct_result)

  sev_train <- sev_train %>%
    dplyr::mutate_at(c("agecat", "veh_body"), as.character)

  no_print(setup2 <- do.call(setup, args))

  actual_result <- vapply(setup2$data_train[setup2$simple_factors], class, character(2))
  actual_result <- unique(as.vector(actual_result))

  expect_equal(actual_result, correct_result)
})

test_that("simple_factors levels encompass all levels across train/test, even if they are not present in test", {

  sev_train <- sev_train %>%
    dplyr::mutate(test_column = as.character(sample(c('a', 'b', 'c', 'd'), size = nrow(.), replace = TRUE)))

  sev_test <- sev_test %>%
    dplyr::mutate(test_column = as.character(sample(c('a', 'b', 'c'), size = nrow(.), replace = TRUE)))

  args <- list(
    data_train = sev_train,
    data_test = sev_test,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  no_print(setup <- do.call(setup, args))

  expect_equal(attr(setup$data_train$test_column, "orig_levels"), c("a", "b", "c", "d"))
  expect_equal(attr(setup$data_test$test_column, "orig_levels"), c("a", "b", "c", "d"))

})


context("Setup - Family")

test_that("family must be one of poisson, gamma or tweedie", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gama',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "'arg' should be one of \"poisson\", \"gamma\", \"tweedie\"")

})

# test_that("poisson family treats weight as offset, if offset is not provided", {
#
#   args <- list(
#     data_train = freq_train,
#     target = 'numclaims',
#     # offset = 'exposure',
#     weight = 'exposure',
#     family = 'poisson',
#     keep_cols = c('pol_nbr', 'premium')
#   )
#
#   expect_message(
#     no_print(setup <- do.call(setup, args)),
#     "No 'offset' provided for family 'poisson', will treat 'weight' as 'offset'"
#   )
#
#   expect_equal(setup$offset, "exposure")
#   expect_equal(setup$weight, "_weight")
#   expect_equal(setup$data_train[[setup$weight]], rep(1, nrow(setup$data_train)))
#
# })

test_that("tweedie_p is ignored when family is poisson or gamma", {

  args <- list(
    data_train = freq_train,
    target = 'numclaims',
    weight = 'exposure',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  args2 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  expect_message(no_print(do.call(setup, args)), "family is 'poisson' or 'gamma', 'tweedie_p' will be ignored")
  expect_message(no_print(do.call(setup, args2)), "family is 'poisson' or 'gamma', 'tweedie_p' will be ignored")

})

test_that("tweedie_p must be provided for family tweedie", {

  args <- list(
    data_train = bc_train,
    target = 'bc',
    weight = 'exposure',
    family = 'tweedie',
    keep_cols = c('pol_nbr', 'premium'),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "Please provide 'tweedie_p'")

})

test_that("tweedie_p must be a numeric scalar", {

  args <- list(
    data_train = bc_train,
    target = 'bc',
    weight = 'exposure',
    family = 'tweedie',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = c(1.5, 1.7),
    folder = tempdir()
  )

  expect_error(do.call(setup, args), "'tweedie_p' must be a numeric scalar")

  args$tweedie_p <- 'whatever'

  expect_error(do.call(setup, args), "'tweedie_p' must be a numeric scalar")

})

test_that("tweedie_p must be in range (1, 2), boundaries excluded", {

  shared_args <- list(
    data_train = bc_train,
    target = 'bc',
    weight = 'exposure',
    family = 'tweedie',
    keep_cols = c('pol_nbr', 'premium'),
    folder = tempdir()
  )

  args1 <- c(
    shared_args,
    list(tweedie_p = 0)
  )

  args2 <- c(
    shared_args,
    list(tweedie_p = 1)
  )

  args3 <- c(
    shared_args,
    list(tweedie_p = 2)
  )

  args4 <- c(
    shared_args,
    list(tweedie_p = 3)
  )

  expect_error(do.call(setup, args1), "'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.", fixed = TRUE)
  expect_error(do.call(setup, args2), "'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.", fixed = TRUE)
  expect_error(do.call(setup, args3), "'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.", fixed = TRUE)
  expect_error(do.call(setup, args4), "'tweedie_p' must be provided and its value in range (1, 2), boundaries excluded.", fixed = TRUE)

})

test_that("family is initialized correctly", {

  args1 <- list(
    data_train = freq_train,
    target = 'numclaims',
    weight = 'exposure',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  args2 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  args3 <- list(
    data_train = bc_train,
    target = 'bc',
    weight = 'exposure',
    family = 'tweedie',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5,
    folder = tempdir()
  )

  # no_print(setup1 <- do.call(setup, args1))
  no_print(setup2 <- do.call(setup, args2))
  no_print(setup3 <- do.call(setup, args3))

  # expect_equal(setup1$family, poisson(link = "log"))
  expect_equal(setup2$family, Gamma(link = "log"))
  expect_equal(setup3$family, statmod::tweedie(var.power = 1.5, link.power = 0))

})


context("Setup - glm backend")

test_that("Glm backend must be either `stats` or `speedglm`", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    glm_backend = "whatever"
  )

  expect_error(no_print({do.call(setup, args)}), "'arg' should be one of \"speedglm\", \"stats\"")
})

test_that("Glm backend results in a correct `glm_fun`", {

  args1 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    glm_backend = "speedglm"
  )

  args2 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    glm_backend = "stats"
  )

  no_print({setup1 <- do.call(setup, args1)})
  no_print({setup2 <- do.call(setup, args2)})

  expect_equal(setup1$glm_fun, speedglm::speedglm)
  expect_equal(setup2$glm_fun, stats::glm)
})

test_that("Glm backends produce identical setup object", {

  args1 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    glm_backend = "speedglm",
    seed = 123
  )

  args2 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    glm_backend = "stats",
    seed = 123
  )

  no_print({setup1 <- do.call(setup, args1)})
  no_print({setup2 <- do.call(setup, args2)})

  setup1$glm_fun <- NULL
  setup1$current_model$glm_fun <- NULL
  setup1$ref_models$intercept_model$glm_fun <- NULL
  setup1$current_model$model_stats$BIC <- NA_real_
  setup1$ref_models$intercept_model$model_stats$BIC <- NA_real_

  setup2$glm_fun <- NULL
  setup2$current_model$glm_fun <- NULL
  setup2$ref_models$intercept_model$glm_fun <- NULL
  setup2$current_model$model_stats$BIC <- NA_real_
  setup2$ref_models$intercept_model$model_stats$BIC <- NA_real_

  expect_equal(setup1, setup2, tolerance = 0.001)
})


context("Setup - folder, loading and saving objects")

test_that("'folder' has to be existing folder", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = "whatever"
  )

  expect_error(no_print({setup <- do.call(setup, args)}), "folder 'whatever' doesn't exist.")
})

test_that("'load_file_nm' has to be existing file", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    load_file_nm = "whatever"
  )

  expect_error(no_print({setup1 <- do.call(setup, args)}))
})

test_that("saving and loading setup objects works", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    load_file_nm = NULL,
    save_file_nm = "test"
  )

  no_print({setup1 <- do.call(setup, args)})

  expected_result <- readr::read_rds(file.path(tempdir(), "test_setup.rds"))

  args$load_file_nm <- "test"
  args$save_file_nm <- NULL

  no_print({setup2 <- do.call(setup, args)})

  expect_equal(setup2, expected_result)
})


context("Setup - seed")

test_that("seed must be numeric scalar", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = tempdir(),
    seed = "whatever"
  )

  expect_error(no_print({setup <- do.call(setup, args)}), "'seed' must be numeric scalar.")
})


context("Setup - integration test")

test_that("setup produces what it should", {

  # set.seed(123)
  #
  # train_rows <- sample(nrow(sev_train), size = 15)
  # dummy_train[train_rows, ]
  #
  # test_subset <- sev_test %>%
  #   dplyr::filter(
  #     pol_yr %in% dummy_train$pol_yr,
  #     gender %in% dummy_train$gender,
  #     agecat %in% dummy_train$agecat,
  #     area %in% dummy_train$area,
  #     veh_body %in% dummy_train$veh_body,
  #     veh_age %in% dummy_train$veh_age,
  #     veh_value %in% dummy_train$veh_value
  #   )
  #
  # test_rows <- sample(nrow(test_subset), size = 5)
  # dummy_test <- test_subset[test_rows, ]
  #
  # rownames(dummy_train) <- NULL
  # rownames(dummy_test) <- NULL
  #
  # dput(dummy_train)
  # dput(dummy_test)
  #
  # Copy & paste from console, hightlight the copied code
  # 'Code' -> 'Reformat Code' (or CTRL + Shift + A)

  dummy_train <-
    structure(
      list(
        pol_nbr = c(
          "334473",
          "245147",
          "387658",
          "352370",
          "377082",
          "154953",
          "335537",
          "424416",
          "279269",
          "457203",
          "347972",
          "425162",
          "450359",
          "141285",
          "406732"
        ),
        pol_yr = structure(
          c(3L, 4L, 3L, 4L, 1L, 5L, 4L, 5L, 1L, 4L, 4L, 4L, 2L, 4L, 3L),
          .Label = c("2000", "2001", "2002", "2003", "2004"), class = "factor"
        ),
        exposure = c(
          0.3011635866,
          0.3367556468,
          0.8405201916,
          0.8021902806,
          0.3668720055,
          0.7446954141,
          0.090349076,
          0.4982888433,
          0.925393566,
          0.1889117043,
          0.8295687885,
          0.3942505133,
          0.7912388775,
          0.9226557153,
          0.6652977413
        ),
        premium = c(
          265.235264554667,
          253.899815785158,
          630.075372211064,
          643.053897524987,
          160.956254138386,
          430.78264233547,
          54.0257808363787,
          340.063462315072,
          608.690463890544,
          216.103123272719,
          1358.75906469446,
          307.498515311814,
          469.706354879722,
          2680.62898468824,
          1726.79635032368
        ),
        gender = structure(
          c(1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L),
          .Label = c("F","M"), class = "factor"
        ),
        agecat = structure(
          c(4L, 2L, 3L, 3L, 4L, 4L, 6L, 2L, 5L, 1L, 1L, 3L, 3L, 2L, 3L),
          .Label = c("1", "2", "3", "4", "5", "6"), class = "factor"
        ),
        area = structure(
          c(5L, 2L, 1L, 3L, 3L, 1L, 4L, 3L, 2L, 1L, 1L, 3L, 3L, 1L, 1L),
          .Label = c("A", "B", "C", "D", "E", "F"),
          class = "factor"
        ),
        veh_body = structure(
          c(10L, 11L, 4L, 10L, 4L, 10L, 10L, 10L, 11L, 4L, 4L, 10L, 4L, 11L, 11L),
          .Label = c(
            "BUS",
            "CONVT",
            "COUPE",
            "HBACK",
            "HDTOP",
            "MCARA",
            "MIBUS",
            "PANVN",
            "RDSTR",
            "SEDAN",
            "STNWG",
            "TRUCK",
            "UTE"
          ),
          class = "factor"
        ),
        veh_age = structure(
          c(1L, 4L, 2L, 2L, 1L, 2L, 1L, 3L, 1L, 3L, 2L, 2L, 1L, 1L, 2L),
          .Label = c("1", "2", "3", "4"), class = "factor"
        ),
        veh_value = structure(
          c(5L, 2L, 4L, 4L, 3L, 4L, 5L, 2L, 5L, 1L, 2L, 4L, 3L, 5L, 5L),
          .Label = c(
            "[0,0.9]",
            "(0.9,1.32]",
            "(1.32,1.71]",
            "(1.71,2.44]",
            "(2.44,34.6]"
          ),
          class = "factor"
        ),
        numclaims = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
        sev = c(
          200,
          1888.829998,
          200,
          200,
          7766.5299988,
          1186.363636,
          480,
          254.090000155,
          353.76999998,
          4450.039978,
          353.76999998,
          353.76999998,
          210.35000229,
          353.76999998,
          383.31999969
        )
      ),
      row.names = c(
        1L,
        2L,
        3L,
        4L,
        5L,
        6L,
        7L,
        8L,
        9L,
        10L,
        11L,
        12L,
        13L,
        14L,
        15L
      ),
      class = "data.frame"
    )

  dummy_test <- structure(
    list(
      pol_nbr = c("214771", "489640", "344317", "487354", "382906"),
      pol_yr = structure(
        c(5L, 1L, 1L, 4L, 4L),
        .Label = c("2000", "2001", "2002", "2003", "2004"),
        class = "factor"
      ),
      exposure = c(
        0.7118412047,
        0.4873374401,
        0.7091033539,
        0.810403833,
        0.925393566
      ),
      premium = c(
        505.854039170545,
        899.575517800543,
        479.327546863998,
        437.188861409991,
        409.00149747586
      ),
      gender = structure(
        c(1L, 2L, 2L, 1L, 1L),
        .Label = c("F", "M"),
        class = "factor"
      ),
      agecat = structure(
        c(3L, 1L, 3L, 2L, 4L),
        .Label = c("1", "2", "3", "4", "5", "6"),
        class = "factor"
      ),
      area = structure(
        c(3L, 2L, 1L, 4L, 3L),
        .Label = c("A", "B", "C", "D", "E", "F"),
        class = "factor"
      ),
      veh_body = structure(
        c(11L, 4L, 4L, 11L, 10L),
        .Label = c(
          "BUS",
          "CONVT",
          "COUPE",
          "HBACK",
          "HDTOP",
          "MCARA",
          "MIBUS",
          "PANVN",
          "RDSTR",
          "SEDAN",
          "STNWG",
          "TRUCK",
          "UTE"
        ),
        class = "factor"
      ),
      veh_age = structure(
        c(3L, 1L, 2L, 4L, 3L),
        .Label = c("1", "2", "3", "4"),
        class = "factor"
      ),
      veh_value = structure(
        c(3L, 3L, 3L, 1L, 3L),
        .Label = c(
          "[0,0.9]",
          "(0.9,1.32]",
          "(1.32,1.71]",
          "(1.71,2.44]",
          "(2.44,34.6]"
        ),
        class = "factor"
      ),
      numclaims = c(1L, 1L, 1L, 1L, 1L),
      sev = c(200, 3981.2299957, 1265.4499969, 2723.0399933, 200)
    ),
    row.names = c(1L, 2L, 3L, 4L, 5L),
    class = "data.frame"
  )

  args <- list(
    data_train = dummy_train,
    data_test = dummy_test,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    folder = '~/insuRglm/tests',
    seed = 123
  )

  # setup <- do.call(insuRglm::setup, args)
  # dput(setup)
  #
  # Copy & paste from console, hightlight the copied code
  # 'Code' -> 'Reformat Code' (or CTRL + Shift + A)

  correct_result <- structure(
    list(
      target = "sev",
      weight = "numclaims",
      offset = NULL,
      family = structure(
        list(
          family = "Gamma",
          link = "log",
          linkfun = function (mu)
            log(mu),
          linkinv = function (eta)
            pmax(exp(eta), .Machine$double.eps),
          variance = function (mu)
            mu ^ 2,
          dev.resids = function (y, mu, wt)
            - 2 * wt * (log(ifelse(y == 0, 1, y /
                                     mu)) - (y - mu) / mu),
          aic = function (y,
                          n, mu, wt, dev)
          {
            n <- sum(wt)
            disp <- dev / n
            - 2 * sum(dgamma(y, 1 / disp, scale = mu * disp, log = TRUE) *
                        wt) + 2
          },
          mu.eta = function (eta)
            pmax(exp(eta), .Machine$double.eps),
          initialize = expression({
            if (any(y <= 0))
              stop("non-positive values not allowed for the 'gamma' family")
            n <-
              rep.int(1, nobs)
            mustart <- y
          }),
          validmu = function (mu)
            all(is.finite(mu)) &&
            all(mu > 0),
          valideta = function (eta)
            TRUE,
          simulate = function (object, nsim)
          {
            wts <- object$prior.weights
            if (any(wts != 1))
              message("using weights as shape parameters")
            ftd <-
              fitted(object)
            shape <-
              MASS::gamma.shape(object)$alpha * wts
            rgamma(nsim * length(ftd), shape = shape, rate = shape /
                     ftd)
          }
        ),
        class = "family"
      ),
      simple_factors = c(
        "pol_yr",
        "gender",
        "agecat",
        "area",
        "veh_body",
        "veh_age",
        "veh_value"
      ),
      glm_fun = function (formula,
                          data,
                          family = gaussian(),
                          weights = NULL,
                          start = NULL,
                          etastart = NULL,
                          mustart = NULL,
                          offset = NULL,
                          maxit = 25,
                          k = 2,
                          sparse = NULL,
                          set.default = list(),
                          trace = FALSE,
                          method = c("eigen", "Cholesky", "qr"),
                          model = FALSE,
                          y = FALSE,
                          fitted = FALSE,
                          ...)
      {
        call <- match.call()
        target <-
          y
        M <-
          match.call(expand.dots = FALSE)
        m <-
          match(c("formula", "data", "subset"), names(M),
                0L)
        M <-
          M[c(1L, m)]
        M$drop.unused.levels <-
          TRUE
        M[[1L]] <-
          quote(stats::model.frame)
        M <-
          eval(M, parent.frame())
        y <-
          M[[1]]
        tf <-
          attr(M, "terms")
        X <-
          model.matrix(tf, M)
        offset <-
          model.offset(M)
        intercept <-
          attributes(tf)$intercept
        set <-
          list(
            sparselim = 0.9,
            camp = 0.01,
            eigendec = TRUE,
            row.chunk = NULL,
            tol.solve = .Machine$double.eps,
            acc = 1e-08,
            tol.values = 1e-07,
            tol.vectors = 1e-07,
            method = match.arg(method)
          )
        nmsC <-
          names(set)
        set[(namc <-
               names(set.default))] <- set.default
        if (length(noNms <-
                   namc[!namc %in% nmsC]) > 0)
          warning("unknown names in set.default: ", paste(noNms,
                                                          collapse = ", "))
        rval <-
          speedglm.wfit(
            y = y,
            X = X,
            family = family,
            weights = weights,
            start = start,
            etastart = etastart,
            mustart = mustart,
            offset = offset,
            intercept = intercept,
            row.chunk = set$row.chunk,
            maxit = maxit,
            k = k,
            acc = set$acc,
            sparselim = set$sparselim,
            camp = set$camp,
            eigendec = set$eigendec,
            tol.solve = set$tol.solve,
            sparse = sparse,
            tol.values = set$tol.values,
            trace = trace,
            tol.vectors = set$tol.vectors,
            method = set$method
          )
        rval$terms <-
          tf
        rval$call <-
          call
        class(rval) <-
          c("speedglm", "speedlm")
        if (model)
          rval$model <-
          M
        if (fitted)
          rval$linear.predictors <-
          predict.speedlm(rval, newdata = M)
        if (target)
          rval$y <-
          y
        if ((rval$iter == maxit) &
            (!rval$convergence))
          warning("Maximum number of iterations reached without convergence")
        rval
      },
      folder = "~/insuRglm/tests",
      base_nm = "sev_gamma",
      seed = 123L,
      obs_avg_tables = list(
        pol_yr = structure(
          list(
            orig_level = c("2000", "2001",
                           "2002", "2003", "2004"),
            weight = c(2L, 1L, 3L, 7L, 3L),
            obs_avg_pred_nonrescaled = c(
              4060.14999939,
              210.35000229,
              261.106666563333,
              1154.31142513429,
              564.84787877
            ),
            obs_avg_lin_nonrescaled = c(
              8.30897519757587,
              5.34877282092315,
              5.56492900798477,
              7.05125927646176,
              6.3365564537795
            ),
            obs_avg_pred_rescaled = c(
              3.51737833567546,
              0.182229853841678,
              0.226201232074748,
              1,
              0.489337510199458
            ),
            obs_avg_lin_rescaled = c(
              1.25771592111411,
              -1.70248645553861,-1.48633026847699,
              0,
              -0.71470282268226
            )
          ),
          row.names = c(NA,-5L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        gender = structure(
          list(
            orig_level = c("F", "M"),
            weight = c(11L, 5L),
            obs_avg_pred_nonrescaled = c(283.357272928182,
                                         3154.35272216),
            obs_avg_lin_nonrescaled = c(5.64670854993465,
                                        8.05653859454848),
            obs_avg_pred_rescaled = c(1, 11.1320690291916),
            obs_avg_lin_rescaled = c(0, 2.40983004461384)
          ),
          row.names = c(NA,-2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        agecat = structure(
          list(
            orig_level = c("1", "2", "3", "4", "5", "6"),
            weight = c(2L,
                       4L, 5L, 3L, 1L, 1L),
            obs_avg_pred_nonrescaled = c(
              2401.90498899,
              687.6949995725,
              269.488000392,
              3050.96454493333,
              353.76999998,
              480
            ),
            obs_avg_lin_nonrescaled = c(
              7.78401744689924,
              6.53334542506553,
              5.59652386391064,
              8.02321306384722,
              5.86864698440522,
              6.17378610390194
            ),
            obs_avg_pred_rescaled = c(
              8.91284578718223,
              2.55185759132938,
              1,
              11.3213372784516,
              1.31274861762083,
              1.78115537352976
            ),
            obs_avg_lin_rescaled = c(
              2.1874935829886,
              0.936821561154892,
              0,
              2.42668919993658,
              0.272123120494585,
              0.577262239991296
            )
          ),
          row.names = c(NA,-6L),
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        area = structure(
          list(
            orig_level = c("A",
                           "B", "C", "D", "E"),
            weight = c(6L, 2L, 6L, 1L, 1L),
            obs_avg_pred_nonrescaled = c(1154.54393560833, 1121.29999899,
                                         1506.47166689667, 480, 200),
            obs_avg_lin_nonrescaled = c(
              7.05146068403262,
              7.02224400456609,
              7.31752555115597,
              6.17378610390194,
              5.29831736654804
            ),
            obs_avg_pred_rescaled = c(
              1,
              0.971206001267663,
              1.30481969584198,
              0.415748578461058,
              0.173228574358774
            ),
            obs_avg_lin_rescaled = c(
              0,
              -0.0292166794665221,
              0.266064867123355,
              -0.87767458013068,
              -1.75314331748458
            )
          ),
          row.names = c(NA,-5L),
          class = c("tbl_df", "tbl",
                    "data.frame")
        ),
        veh_body = structure(
          list(
            orig_level = c("HBACK",
                           "SEDAN", "STNWG"),
            weight = c(5L, 7L, 4L),
            obs_avg_pred_nonrescaled = c(2596.137995814,
                                         418.33051947, 744.9224994125),
            obs_avg_lin_nonrescaled = c(7.86178023350441,
                                        6.03627183650862, 6.61328018533408),
            obs_avg_pred_rescaled = c(6.20594930320444,
                                      1, 1.78070321131787),
            obs_avg_lin_rescaled = c(1.82550839699579,
                                     0, 0.577008348825463)
          ),
          row.names = c(NA,-3L),
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        veh_age = structure(
          list(
            orig_level = c("1",
                           "2", "3", "4"),
            weight = c(6L, 6L, 3L, 1L),
            obs_avg_pred_nonrescaled = c(1560.73666684167,
                                         446.203939275, 1652.73999277, 1888.829998),
            obs_avg_lin_nonrescaled = c(
              7.35291321111611,
              6.10077611045565,
              7.41018979129897,
              7.54371286768669
            ),
            obs_avg_pred_rescaled = c(1, 0.285893161066015, 1.05894865410865,
                                      1.21021696877428),
            obs_avg_lin_rescaled = c(0,-1.25213710066045,
                                     0.0572765801828661, 0.190799656570587)
          ),
          row.names = c(NA,-4L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        veh_value = structure(
          list(
            orig_level = c(
              "[0,0.9]",
              "(0.9,1.32]",
              "(1.32,1.71]",
              "(1.71,2.44]",
              "(2.44,34.6]"
            ),
            weight = c(1L, 4L,
                       2L, 4L, 5L),
            obs_avg_pred_nonrescaled = c(
              4450.039978,
              687.6949995725,
              3988.440000545,
              485.033408995,
              354.17199993
            ),
            obs_avg_lin_nonrescaled = c(
              8.40066835894016,
              6.53334542506553,
              8.29115545612534,
              6.18421777309083,
              5.86978267064296
            ),
            obs_avg_pred_rescaled = c(
              12.5646295553559,
              1.94169781831545,
              11.2613080687725,
              1.36948547341649,
              1
            ),
            obs_avg_lin_rescaled = c(
              2.53088568829719,
              0.663562754422568,
              2.42137278548237,
              0.314435102447862,
              0
            )
          ),
          row.names = c(NA,-5L),
          class = c("tbl_df", "tbl", "data.frame")
        )
      ),
      data_train = structure(
        list(
          sev = c(
            200,
            1888.829998,
            200,
            200,
            7766.5299988,
            1186.363636,
            480,
            254.090000155,
            353.76999998,
            4450.039978,
            353.76999998,
            353.76999998,
            210.35000229,
            353.76999998,
            383.31999969
          ),
          numclaims = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
                        1L, 1L, 1L, 1L, 1L, 1L),
          pol_nbr = c(
            "334473",
            "245147",
            "387658",
            "352370",
            "377082",
            "154953",
            "335537",
            "424416",
            "279269",
            "457203",
            "347972",
            "425162",
            "450359",
            "141285",
            "406732"
          ),
          exposure = c(
            0.3011635866,
            0.3367556468,
            0.8405201916,
            0.8021902806,
            0.3668720055,
            0.7446954141,
            0.090349076,
            0.4982888433,
            0.925393566,
            0.1889117043,
            0.8295687885,
            0.3942505133,
            0.7912388775,
            0.9226557153,
            0.6652977413
          ),
          premium = c(
            265.235264554667,
            253.899815785158,
            630.075372211064,
            643.053897524987,
            160.956254138386,
            430.78264233547,
            54.0257808363787,
            340.063462315072,
            608.690463890544,
            216.103123272719,
            1358.75906469446,
            307.498515311814,
            469.706354879722,
            2680.62898468824,
            1726.79635032368
          ),
          pol_yr = structure(
            c(3L, 4L, 3L, 4L, 1L, 5L, 4L, 5L,
              1L, 4L, 4L, 4L, 2L, 4L, 3L),
            .Label = c("2000", "2001",
                       "2002", "2003", "2004"),
            class = c("simple_factor", "factor"),
            var_nm = "pol_yr",
            orig_levels = c("2000", "2001",
                            "2002", "2003", "2004"),
            base_level = "2003"
          ),
          gender = structure(
            c(1L,
              2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L),
            .Label = c("F", "M"),
            class = c("simple_factor", "factor"),
            var_nm = "gender",
            orig_levels = c("F", "M"),
            base_level = "F"
          ),
          agecat = structure(
            c(4L, 2L, 3L, 3L, 4L, 4L, 6L, 2L,
              5L, 1L, 1L, 3L, 3L, 2L, 3L),
            .Label = c("1", "2", "3",
                       "4", "5", "6"),
            class = c("simple_factor", "factor"),
            var_nm = "agecat",
            orig_levels = c("1",
                            "2", "3", "4", "5", "6"),
            base_level = "3"
          ),
          area = structure(
            c(5L,
              2L, 1L, 3L, 3L, 1L, 4L, 3L, 2L, 1L, 1L, 3L, 3L, 1L, 1L),
            .Label = c("A", "B", "C", "D", "E", "F"),
            class = c("simple_factor",
                      "factor"),
            var_nm = "area",
            orig_levels = c("A", "B",
                            "C", "D", "E", "F"),
            base_level = "A"
          ),
          veh_body = structure(
            c(10L,
              11L, 4L, 10L, 4L, 10L, 10L, 10L, 11L, 4L, 4L, 10L, 4L,
              11L, 11L),
            .Label = c(
              "BUS",
              "CONVT",
              "COUPE",
              "HBACK",
              "HDTOP",
              "MCARA",
              "MIBUS",
              "PANVN",
              "RDSTR",
              "SEDAN",
              "STNWG",
              "TRUCK",
              "UTE"
            ),
            class = c("simple_factor",
                      "factor"),
            var_nm = "veh_body",
            orig_levels = c(
              "BUS",
              "CONVT",
              "COUPE",
              "HBACK",
              "HDTOP",
              "MCARA",
              "MIBUS",
              "PANVN",
              "RDSTR",
              "SEDAN",
              "STNWG",
              "TRUCK",
              "UTE"
            ),
            base_level = "SEDAN"
          ),
          veh_age = structure(
            c(1L, 4L, 2L, 2L, 1L, 2L, 1L, 3L,
              1L, 3L, 2L, 2L, 1L, 1L, 2L),
            .Label = c("1", "2", "3",
                       "4"),
            class = c("simple_factor", "factor"),
            var_nm = "veh_age",
            orig_levels = c("1",
                            "2", "3", "4"),
            base_level = "1"
          ),
          veh_value = structure(
            c(5L,
              2L, 4L, 4L, 3L, 4L, 5L, 2L, 5L, 1L, 2L, 4L, 3L, 5L, 5L),
            .Label = c(
              "[0,0.9]",
              "(0.9,1.32]",
              "(1.32,1.71]",
              "(1.71,2.44]",
              "(2.44,34.6]"
            ),
            class = c("simple_factor",
                      "factor"),
            var_nm = "veh_value",
            orig_levels = c(
              "[0,0.9]",
              "(0.9,1.32]",
              "(1.32,1.71]",
              "(1.71,2.44]",
              "(2.44,34.6]"
            ),
            base_level = "(2.44,34.6]"
          )
        ),
        row.names = c(NA,-15L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      data_test = structure(
        list(
          sev = c(200, 3981.2299957, 1265.4499969, 2723.0399933,
                  200),
          numclaims = c(1L, 1L, 1L, 1L, 1L),
          pol_nbr = c("214771",
                      "489640", "344317", "487354", "382906"),
          exposure = c(
            0.7118412047,
            0.4873374401,
            0.7091033539,
            0.810403833,
            0.925393566
          ),
          premium = c(
            505.854039170545,
            899.575517800543,
            479.327546863998,
            437.188861409991,
            409.00149747586
          ),
          pol_yr = structure(
            c(5L,
              1L, 1L, 4L, 4L),
            .Label = c("2000", "2001", "2002", "2003",
                       "2004"),
            class = c("simple_factor", "factor"),
            var_nm = "pol_yr",
            orig_levels = c("2000",
                            "2001", "2002", "2003", "2004"),
            base_level = "2003"
          ),
          gender = structure(
            c(1L, 2L, 2L, 1L, 1L),
            .Label = c("F",
                       "M"),
            class = c("simple_factor", "factor"),
            var_nm = "gender",
            orig_levels = c("F",
                            "M"),
            base_level = "F"
          ),
          agecat = structure(
            c(3L, 1L,
              3L, 2L, 4L),
            .Label = c("1", "2", "3", "4", "5", "6"),
            class = c("simple_factor",
                      "factor"),
            var_nm = "agecat",
            orig_levels = c("1", "2",
                            "3", "4", "5", "6"),
            base_level = "3"
          ),
          area = structure(
            c(3L,
              2L, 1L, 4L, 3L),
            .Label = c("A", "B", "C", "D", "E",
                       "F"),
            class = c("simple_factor", "factor"),
            var_nm = "area",
            orig_levels = c("A",
                            "B", "C", "D", "E", "F"),
            base_level = "A"
          ),
          veh_body = structure(
            c(11L,
              4L, 4L, 11L, 10L),
            .Label = c(
              "BUS",
              "CONVT",
              "COUPE",
              "HBACK",
              "HDTOP",
              "MCARA",
              "MIBUS",
              "PANVN",
              "RDSTR",
              "SEDAN",
              "STNWG",
              "TRUCK",
              "UTE"
            ),
            class = c("simple_factor",
                      "factor"),
            var_nm = "veh_body",
            orig_levels = c(
              "BUS",
              "CONVT",
              "COUPE",
              "HBACK",
              "HDTOP",
              "MCARA",
              "MIBUS",
              "PANVN",
              "RDSTR",
              "SEDAN",
              "STNWG",
              "TRUCK",
              "UTE"
            ),
            base_level = "SEDAN"
          ),
          veh_age = structure(
            c(3L, 1L, 2L, 4L, 3L),
            .Label = c("1",
                       "2", "3", "4"),
            class = c("simple_factor", "factor"),
            var_nm = "veh_age",
            orig_levels = c("1",
                            "2", "3", "4"),
            base_level = "1"
          ),
          veh_value = structure(
            c(3L,
              3L, 3L, 1L, 3L),
            .Label = c(
              "[0,0.9]",
              "(0.9,1.32]",
              "(1.32,1.71]",
              "(1.71,2.44]",
              "(2.44,34.6]"
            ),
            class = c("simple_factor",
                      "factor"),
            var_nm = "veh_value",
            orig_levels = c(
              "[0,0.9]",
              "(0.9,1.32]",
              "(1.32,1.71]",
              "(1.71,2.44]",
              "(2.44,34.6]"
            ),
            base_level = "(2.44,34.6]"
          )
        ),
        row.names = c(NA,-5L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      current_model = structure(
        list(
          target = "sev",
          weight = "numclaims",
          offset = NULL,
          family = structure(
            list(
              family = "Gamma",
              link = "log",
              linkfun = function (mu)
                log(mu),
              linkinv = function (eta)
                pmax(exp(eta), .Machine$double.eps),
              variance = function (mu)
                mu ^
                2,
              dev.resids = function (y, mu, wt)
                -
                2 * wt * (log(ifelse(y == 0, 1, y / mu)) - (y - mu) / mu),
              aic = function (y, n, mu, wt, dev)
              {
                n <- sum(wt)
                disp <-
                  dev / n
                -
                  2 * sum(dgamma(y, 1 / disp, scale = mu * disp,
                                 log = TRUE) * wt) + 2
              },
              mu.eta = function (eta)
                pmax(exp(eta), .Machine$double.eps),
              initialize = expression({
                if (any(y <= 0))
                  stop("non-positive values not allowed for the 'gamma' family")
                n <-
                  rep.int(1, nobs)
                mustart <-
                  y
              }),
              validmu = function (mu)
                all(is.finite(mu)) &&
                all(mu > 0),
              valideta = function (eta)
                TRUE,
              simulate = function (object, nsim)
              {
                wts <- object$prior.weights
                if (any(wts != 1))
                  message("using weights as shape parameters")
                ftd <-
                  fitted(object)
                shape <-
                  MASS::gamma.shape(object)$alpha * wts
                rgamma(nsim * length(ftd), shape = shape, rate = shape /
                         ftd)
              }
            ),
            class = "family"
          ),
          predictors = NULL,
          .predictors = NULL,
          data_attrs = list(
            pol_yr = list(
              levels = c("2000", "2001", "2002",
                         "2003", "2004"),
              class = c("simple_factor", "factor"),
              var_nm = "pol_yr",
              orig_levels = c("2000", "2001",
                              "2002", "2003", "2004"),
              base_level = "2003"
            ),
            gender = list(
              levels = c("F", "M"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "gender",
              orig_levels = c("F",
                              "M"),
              base_level = "F"
            ),
            agecat = list(
              levels = c("1",
                         "2", "3", "4", "5", "6"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "agecat",
              orig_levels = c("1",
                              "2", "3", "4", "5", "6"),
              base_level = "3"
            ),
            area = list(
              levels = c("A", "B", "C", "D", "E", "F"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "area",
              orig_levels = c("A",
                              "B", "C", "D", "E", "F"),
              base_level = "A"
            ),
            veh_body = list(
              levels = c(
                "BUS",
                "CONVT",
                "COUPE",
                "HBACK",
                "HDTOP",
                "MCARA",
                "MIBUS",
                "PANVN",
                "RDSTR",
                "SEDAN",
                "STNWG",
                "TRUCK",
                "UTE"
              ),
              class = c("simple_factor",
                        "factor"),
              var_nm = "veh_body",
              orig_levels = c(
                "BUS",
                "CONVT",
                "COUPE",
                "HBACK",
                "HDTOP",
                "MCARA",
                "MIBUS",
                "PANVN",
                "RDSTR",
                "SEDAN",
                "STNWG",
                "TRUCK",
                "UTE"
              ),
              base_level = "SEDAN"
            ),
            veh_age = list(
              levels = c("1",
                         "2", "3", "4"),
              class = c("simple_factor", "factor"),
              var_nm = "veh_age",
              orig_levels = c("1", "2",
                              "3", "4"),
              base_level = "1"
            ),
            veh_value = list(
              levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              class = c("simple_factor", "factor"),
              var_nm = "veh_value",
              orig_levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              base_level = "(2.44,34.6]"
            )
          ),
          glm_fun = function (formula,
                              data,
                              family = gaussian(),
                              weights = NULL,
                              start = NULL,
                              etastart = NULL,
                              mustart = NULL,
                              offset = NULL,
                              maxit = 25,
                              k = 2,
                              sparse = NULL,
                              set.default = list(),
                              trace = FALSE,
                              method = c("eigen",
                                         "Cholesky", "qr"),
                              model = FALSE,
                              y = FALSE,
                              fitted = FALSE,
                              ...)
          {
            call <- match.call()
            target <-
              y
            M <-
              match.call(expand.dots = FALSE)
            m <-
              match(c("formula", "data", "subset"), names(M),
                    0L)
            M <-
              M[c(1L, m)]
            M$drop.unused.levels <-
              TRUE
            M[[1L]] <-
              quote(stats::model.frame)
            M <-
              eval(M, parent.frame())
            y <-
              M[[1]]
            tf <-
              attr(M, "terms")
            X <-
              model.matrix(tf, M)
            offset <-
              model.offset(M)
            intercept <-
              attributes(tf)$intercept
            set <-
              list(
                sparselim = 0.9,
                camp = 0.01,
                eigendec = TRUE,
                row.chunk = NULL,
                tol.solve = .Machine$double.eps,
                acc = 1e-08,
                tol.values = 1e-07,
                tol.vectors = 1e-07,
                method = match.arg(method)
              )
            nmsC <-
              names(set)
            set[(namc <-
                   names(set.default))] <- set.default
            if (length(noNms <-
                       namc[!namc %in% nmsC]) > 0)
              warning("unknown names in set.default: ", paste(noNms,
                                                              collapse = ", "))
            rval <-
              speedglm.wfit(
                y = y,
                X = X,
                family = family,
                weights = weights,
                start = start,
                etastart = etastart,
                mustart = mustart,
                offset = offset,
                intercept = intercept,
                row.chunk = set$row.chunk,
                maxit = maxit,
                k = k,
                acc = set$acc,
                sparselim = set$sparselim,
                camp = set$camp,
                eigendec = set$eigendec,
                tol.solve = set$tol.solve,
                sparse = sparse,
                tol.values = set$tol.values,
                trace = trace,
                tol.vectors = set$tol.vectors,
                method = set$method
              )
            rval$terms <-
              tf
            rval$call <-
              call
            class(rval) <-
              c("speedglm", "speedlm")
            if (model)
              rval$model <-
              M
            if (fitted)
              rval$linear.predictors <-
              predict.speedlm(rval,
                              newdata = M)
            if (target)
              rval$y <-
              y
            if ((rval$iter == maxit) &
                (!rval$convergence))
              warning("Maximum number of iterations reached without convergence")
            rval
          },
          betas = structure(
            list(
              factor = "(Intercept)",
              actual_level = "(Intercept)",
              estimate = 7.0737301,
              std_error = 0.4526683,
              std_error_pct = "6%"
            ),
            row.names = c(NA,-1L),
            class = c("tbl_df", "tbl", "data.frame")
          ),
          beta_triangles = list(),
          model_stats = structure(
            list(
              null.deviance = 26.5654539001954,
              df.null = 14L,
              logLik = -128.735038997948,
              AIC = 261.470077995896,
              BIC = NA_real_,
              deviance = 26.5654539001954,
              df.residual = 14L,
              dispersion = 3.27853790895237
            ),
            row.names = c(NA,-1L),
            class = c("tbl_df", "tbl", "data.frame")
          ),
          current_baseline = 7.0737301,
          factor_tables = list(
            pol_yr = structure(
              list(
                factor = c("pol_yr",
                           "pol_yr", "pol_yr", "pol_yr", "pol_yr"),
                orig_level = c("2000",
                               "2001", "2002", "2003", "2004"),
                actual_level = c("2000",
                                 "2001", "2002", "2003", "2004"),
                weight = c(2L, 1L, 3L,
                           7L, 3L),
                obs_avg_pred_nonrescaled = c(
                  4060.14999939,
                  210.35000229,
                  261.106666563333,
                  1154.31142513429,
                  564.84787877
                ),
                obs_avg_lin_nonrescaled = c(
                  8.30897519757587,
                  5.34877282092315,
                  5.56492900798477,
                  7.05125927646176,
                  6.3365564537795
                ),
                obs_avg_pred_rescaled = c(
                  3.51737833567546,
                  0.182229853841678,
                  0.226201232074748,
                  1,
                  0.489337510199458
                ),
                obs_avg_lin_rescaled = c(
                  1.25771592111411,-1.70248645553861,
                  -1.48633026847699,
                  0,
                  -0.71470282268226
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "")
              ),
              row.names = c(NA,-5L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            gender = structure(
              list(
                factor = c("gender",
                           "gender"),
                orig_level = c("F", "M"),
                actual_level = c("F",
                                 "M"),
                weight = c(11L, 5L),
                obs_avg_pred_nonrescaled = c(283.357272928182,
                                             3154.35272216),
                obs_avg_lin_nonrescaled = c(5.64670854993465,
                                            8.05653859454848),
                obs_avg_pred_rescaled = c(1, 11.1320690291916),
                obs_avg_lin_rescaled = c(0, 2.40983004461384),
                fitted_avg_pred_nonrescaled = c(1180.54335085709,
                                                1180.54335085709),
                fitted_avg_lin_nonrescaled = c(7.07373007830543,
                                               7.07373007830543),
                fitted_avg_pred_rescaled = c(1, 1),
                fitted_avg_lin_rescaled = c(0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_),
                model_avg_lin_rescaled = c(NA_real_, NA_real_),
                geom_text_label = c("", "")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df", "tbl", "data.frame")
            ),
            agecat = structure(
              list(
                factor = c("agecat", "agecat", "agecat", "agecat",
                           "agecat", "agecat"),
                orig_level = c("1", "2", "3",
                               "4", "5", "6"),
                actual_level = c("1", "2", "3", "4",
                                 "5", "6"),
                weight = c(2L, 4L, 5L, 3L, 1L, 1L),
                obs_avg_pred_nonrescaled = c(
                  2401.90498899,
                  687.6949995725,
                  269.488000392,
                  3050.96454493333,
                  353.76999998,
                  480
                ),
                obs_avg_lin_nonrescaled = c(
                  7.78401744689924,
                  6.53334542506553,
                  5.59652386391064,
                  8.02321306384722,
                  5.86864698440522,
                  6.17378610390194
                ),
                obs_avg_pred_rescaled = c(
                  8.91284578718223,
                  2.55185759132938,
                  1,
                  11.3213372784516,
                  1.31274861762083,
                  1.78115537352976
                ),
                obs_avg_lin_rescaled = c(
                  2.1874935829886,
                  0.936821561154892,
                  0,
                  2.42668919993658,
                  0.272123120494585,
                  0.577262239991296
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0,
                                            0, 0, 0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_, NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_, NA_real_, NA_real_,
                                           NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "", "")
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            area = structure(
              list(
                factor = c("area",
                           "area", "area", "area", "area", "area"),
                orig_level = c("A",
                               "B", "C", "D", "E", "F"),
                actual_level = c("A", "B",
                                 "C", "D", "E", "F"),
                weight = c(6L, 2L, 6L, 1L, 1L, NA),
                obs_avg_pred_nonrescaled = c(1154.54393560833, 1121.29999899,
                                             1506.47166689667, 480, 200, NA),
                obs_avg_lin_nonrescaled = c(
                  7.05146068403262,
                  7.02224400456609,
                  7.31752555115597,
                  6.17378610390194,
                  5.29831736654804,
                  NA
                ),
                obs_avg_pred_rescaled = c(
                  1,
                  0.971206001267663,
                  1.30481969584198,
                  0.415748578461058,
                  0.173228574358774,
                  NA
                ),
                obs_avg_lin_rescaled = c(
                  0,
                  -0.0292166794665221,
                  0.266064867123355,
                  -0.87767458013068,
                  -1.75314331748458,
                  NA
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  NA
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  NA
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1, NA),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0, NA),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "", "")
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            veh_body = structure(
              list(
                factor = c(
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body"
                ),
                orig_level = c(
                  "BUS",
                  "CONVT",
                  "COUPE",
                  "HBACK",
                  "HDTOP",
                  "MCARA",
                  "MIBUS",
                  "PANVN",
                  "RDSTR",
                  "SEDAN",
                  "STNWG",
                  "TRUCK",
                  "UTE"
                ),
                actual_level = c(
                  "BUS",
                  "CONVT",
                  "COUPE",
                  "HBACK",
                  "HDTOP",
                  "MCARA",
                  "MIBUS",
                  "PANVN",
                  "RDSTR",
                  "SEDAN",
                  "STNWG",
                  "TRUCK",
                  "UTE"
                ),
                weight = c(NA, NA, NA, 5L, NA, NA, NA, NA, NA, 7L,
                           4L, NA, NA),
                obs_avg_pred_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  2596.137995814,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  418.33051947,
                  744.9224994125,
                  NA,
                  NA
                ),
                obs_avg_lin_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  7.86178023350441,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  6.03627183650862,
                  6.61328018533408,
                  NA,
                  NA
                ),
                obs_avg_pred_rescaled = c(
                  NA,
                  NA,
                  NA,
                  6.20594930320444,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  1,
                  1.78070321131787,
                  NA,
                  NA
                ),
                obs_avg_lin_rescaled = c(
                  NA,
                  NA,
                  NA,
                  1.82550839699579,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  0,
                  0.577008348825463,
                  NA,
                  NA
                ),
                fitted_avg_pred_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  1180.54335085709,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  1180.54335085709,
                  1180.54335085709,
                  NA,
                  NA
                ),
                fitted_avg_lin_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  7.07373007830543,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  7.07373007830543,
                  7.07373007830543,
                  NA,
                  NA
                ),
                fitted_avg_pred_rescaled = c(NA,
                                             NA, NA, 1, NA, NA, NA, NA, NA, 1, 1, NA, NA),
                fitted_avg_lin_rescaled = c(NA,
                                            NA, NA, 0, NA, NA, NA, NA, NA, 0, 0, NA, NA),
                model_avg_pred_nonrescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_lin_nonrescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_pred_rescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_lin_rescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                geom_text_label = c("", "",
                                    "", "", "", "", "", "", "", "", "", "", "")
              ),
              row.names = c(NA,-13L),
              class = c("tbl_df", "tbl", "data.frame")
            ),
            veh_age = structure(
              list(
                factor = c("veh_age", "veh_age", "veh_age", "veh_age"),
                orig_level = c("1", "2", "3", "4"),
                actual_level = c("1",
                                 "2", "3", "4"),
                weight = c(6L, 6L, 3L, 1L),
                obs_avg_pred_nonrescaled = c(1560.73666684167,
                                             446.203939275, 1652.73999277, 1888.829998),
                obs_avg_lin_nonrescaled = c(
                  7.35291321111611,
                  6.10077611045565,
                  7.41018979129897,
                  7.54371286768669
                ),
                obs_avg_pred_rescaled = c(1, 0.285893161066015,
                                          1.05894865410865, 1.21021696877428),
                obs_avg_lin_rescaled = c(0,-1.25213710066045, 0.0572765801828661, 0.190799656570587),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0,
                                            0, 0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "")
              ),
              row.names = c(NA,-4L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            veh_value = structure(
              list(
                factor = c(
                  "veh_value",
                  "veh_value",
                  "veh_value",
                  "veh_value",
                  "veh_value"
                ),
                orig_level = c(
                  "[0,0.9]",
                  "(0.9,1.32]",
                  "(1.32,1.71]",
                  "(1.71,2.44]",
                  "(2.44,34.6]"
                ),
                actual_level = c(
                  "[0,0.9]",
                  "(0.9,1.32]",
                  "(1.32,1.71]",
                  "(1.71,2.44]",
                  "(2.44,34.6]"
                ),
                weight = c(1L, 4L, 2L, 4L, 5L),
                obs_avg_pred_nonrescaled = c(
                  4450.039978,
                  687.6949995725,
                  3988.440000545,
                  485.033408995,
                  354.17199993
                ),
                obs_avg_lin_nonrescaled = c(
                  8.40066835894016,
                  6.53334542506553,
                  8.29115545612534,
                  6.18421777309083,
                  5.86978267064296
                ),
                obs_avg_pred_rescaled = c(
                  12.5646295553559,
                  1.94169781831545,
                  11.2613080687725,
                  1.36948547341649,
                  1
                ),
                obs_avg_lin_rescaled = c(
                  2.53088568829719,
                  0.663562754422568,
                  2.42137278548237,
                  0.314435102447862,
                  0
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "")
              ),
              row.names = c(NA,-5L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          relativities = list(`base_value` = 1180.54337646846),
          train_predictions = c(
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709
          ),
          test_predictions = NULL,
          cv_predictions = NULL
        ),
        class = "fitted_model"
      ),
      ref_models = list(intercept_model = structure(
        list(
          target = "sev",
          weight = "numclaims",
          offset = NULL,
          family = structure(
            list(
              family = "Gamma",
              link = "log",
              linkfun = function (mu)
                log(mu),
              linkinv = function (eta)
                pmax(exp(eta), .Machine$double.eps),
              variance = function (mu)
                mu ^
                2,
              dev.resids = function (y, mu, wt)
                -
                2 * wt * (log(ifelse(y == 0, 1, y / mu)) - (y - mu) / mu),
              aic = function (y, n, mu, wt, dev)
              {
                n <- sum(wt)
                disp <-
                  dev / n
                -
                  2 * sum(dgamma(y, 1 / disp, scale = mu * disp,
                                 log = TRUE) * wt) + 2
              },
              mu.eta = function (eta)
                pmax(exp(eta), .Machine$double.eps),
              initialize = expression({
                if (any(y <= 0))
                  stop("non-positive values not allowed for the 'gamma' family")
                n <-
                  rep.int(1, nobs)
                mustart <-
                  y
              }),
              validmu = function (mu)
                all(is.finite(mu)) &&
                all(mu > 0),
              valideta = function (eta)
                TRUE,
              simulate = function (object, nsim)
              {
                wts <- object$prior.weights
                if (any(wts != 1))
                  message("using weights as shape parameters")
                ftd <-
                  fitted(object)
                shape <-
                  MASS::gamma.shape(object)$alpha * wts
                rgamma(nsim * length(ftd), shape = shape, rate = shape /
                         ftd)
              }
            ),
            class = "family"
          ),
          predictors = NULL,
          .predictors = NULL,
          data_attrs = list(
            pol_yr = list(
              levels = c("2000", "2001", "2002",
                         "2003", "2004"),
              class = c("simple_factor", "factor"),
              var_nm = "pol_yr",
              orig_levels = c("2000", "2001",
                              "2002", "2003", "2004"),
              base_level = "2003"
            ),
            gender = list(
              levels = c("F", "M"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "gender",
              orig_levels = c("F",
                              "M"),
              base_level = "F"
            ),
            agecat = list(
              levels = c("1",
                         "2", "3", "4", "5", "6"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "agecat",
              orig_levels = c("1",
                              "2", "3", "4", "5", "6"),
              base_level = "3"
            ),
            area = list(
              levels = c("A", "B", "C", "D", "E", "F"),
              class = c("simple_factor",
                        "factor"),
              var_nm = "area",
              orig_levels = c("A",
                              "B", "C", "D", "E", "F"),
              base_level = "A"
            ),
            veh_body = list(
              levels = c(
                "BUS",
                "CONVT",
                "COUPE",
                "HBACK",
                "HDTOP",
                "MCARA",
                "MIBUS",
                "PANVN",
                "RDSTR",
                "SEDAN",
                "STNWG",
                "TRUCK",
                "UTE"
              ),
              class = c("simple_factor",
                        "factor"),
              var_nm = "veh_body",
              orig_levels = c(
                "BUS",
                "CONVT",
                "COUPE",
                "HBACK",
                "HDTOP",
                "MCARA",
                "MIBUS",
                "PANVN",
                "RDSTR",
                "SEDAN",
                "STNWG",
                "TRUCK",
                "UTE"
              ),
              base_level = "SEDAN"
            ),
            veh_age = list(
              levels = c("1",
                         "2", "3", "4"),
              class = c("simple_factor", "factor"),
              var_nm = "veh_age",
              orig_levels = c("1", "2",
                              "3", "4"),
              base_level = "1"
            ),
            veh_value = list(
              levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              class = c("simple_factor", "factor"),
              var_nm = "veh_value",
              orig_levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              base_level = "(2.44,34.6]"
            )
          ),
          glm_fun = function (formula,
                              data,
                              family = gaussian(),
                              weights = NULL,
                              start = NULL,
                              etastart = NULL,
                              mustart = NULL,
                              offset = NULL,
                              maxit = 25,
                              k = 2,
                              sparse = NULL,
                              set.default = list(),
                              trace = FALSE,
                              method = c("eigen",
                                         "Cholesky", "qr"),
                              model = FALSE,
                              y = FALSE,
                              fitted = FALSE,
                              ...)
          {
            call <- match.call()
            target <-
              y
            M <-
              match.call(expand.dots = FALSE)
            m <-
              match(c("formula", "data", "subset"), names(M),
                    0L)
            M <-
              M[c(1L, m)]
            M$drop.unused.levels <-
              TRUE
            M[[1L]] <-
              quote(stats::model.frame)
            M <-
              eval(M, parent.frame())
            y <-
              M[[1]]
            tf <-
              attr(M, "terms")
            X <-
              model.matrix(tf, M)
            offset <-
              model.offset(M)
            intercept <-
              attributes(tf)$intercept
            set <-
              list(
                sparselim = 0.9,
                camp = 0.01,
                eigendec = TRUE,
                row.chunk = NULL,
                tol.solve = .Machine$double.eps,
                acc = 1e-08,
                tol.values = 1e-07,
                tol.vectors = 1e-07,
                method = match.arg(method)
              )
            nmsC <-
              names(set)
            set[(namc <-
                   names(set.default))] <- set.default
            if (length(noNms <-
                       namc[!namc %in% nmsC]) > 0)
              warning("unknown names in set.default: ", paste(noNms,
                                                              collapse = ", "))
            rval <-
              speedglm.wfit(
                y = y,
                X = X,
                family = family,
                weights = weights,
                start = start,
                etastart = etastart,
                mustart = mustart,
                offset = offset,
                intercept = intercept,
                row.chunk = set$row.chunk,
                maxit = maxit,
                k = k,
                acc = set$acc,
                sparselim = set$sparselim,
                camp = set$camp,
                eigendec = set$eigendec,
                tol.solve = set$tol.solve,
                sparse = sparse,
                tol.values = set$tol.values,
                trace = trace,
                tol.vectors = set$tol.vectors,
                method = set$method
              )
            rval$terms <-
              tf
            rval$call <-
              call
            class(rval) <-
              c("speedglm", "speedlm")
            if (model)
              rval$model <-
              M
            if (fitted)
              rval$linear.predictors <-
              predict.speedlm(rval,
                              newdata = M)
            if (target)
              rval$y <-
              y
            if ((rval$iter == maxit) &
                (!rval$convergence))
              warning("Maximum number of iterations reached without convergence")
            rval
          },
          betas = structure(
            list(
              factor = "(Intercept)",
              actual_level = "(Intercept)",
              estimate = 7.0737301,
              std_error = 0.4526683,
              std_error_pct = "6%"
            ),
            row.names = c(NA,-1L),
            class = c("tbl_df", "tbl", "data.frame")
          ),
          beta_triangles = list(),
          model_stats = structure(
            list(
              null.deviance = 26.5654539001954,
              df.null = 14L,
              logLik = -128.735038997948,
              AIC = 261.470077995896,
              BIC = NA_real_,
              deviance = 26.5654539001954,
              df.residual = 14L,
              dispersion = 3.27853790895237
            ),
            row.names = c(NA,-1L),
            class = c("tbl_df", "tbl", "data.frame")
          ),
          current_baseline = 7.0737301,
          factor_tables = list(
            pol_yr = structure(
              list(
                factor = c("pol_yr",
                           "pol_yr", "pol_yr", "pol_yr", "pol_yr"),
                orig_level = c("2000",
                               "2001", "2002", "2003", "2004"),
                actual_level = c("2000",
                                 "2001", "2002", "2003", "2004"),
                weight = c(2L, 1L, 3L,
                           7L, 3L),
                obs_avg_pred_nonrescaled = c(
                  4060.14999939,
                  210.35000229,
                  261.106666563333,
                  1154.31142513429,
                  564.84787877
                ),
                obs_avg_lin_nonrescaled = c(
                  8.30897519757587,
                  5.34877282092315,
                  5.56492900798477,
                  7.05125927646176,
                  6.3365564537795
                ),
                obs_avg_pred_rescaled = c(
                  3.51737833567546,
                  0.182229853841678,
                  0.226201232074748,
                  1,
                  0.489337510199458
                ),
                obs_avg_lin_rescaled = c(
                  1.25771592111411,-1.70248645553861,
                  -1.48633026847699,
                  0,
                  -0.71470282268226
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "")
              ),
              row.names = c(NA,-5L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            gender = structure(
              list(
                factor = c("gender",
                           "gender"),
                orig_level = c("F", "M"),
                actual_level = c("F",
                                 "M"),
                weight = c(11L, 5L),
                obs_avg_pred_nonrescaled = c(283.357272928182,
                                             3154.35272216),
                obs_avg_lin_nonrescaled = c(5.64670854993465,
                                            8.05653859454848),
                obs_avg_pred_rescaled = c(1, 11.1320690291916),
                obs_avg_lin_rescaled = c(0, 2.40983004461384),
                fitted_avg_pred_nonrescaled = c(1180.54335085709,
                                                1180.54335085709),
                fitted_avg_lin_nonrescaled = c(7.07373007830543,
                                               7.07373007830543),
                fitted_avg_pred_rescaled = c(1, 1),
                fitted_avg_lin_rescaled = c(0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_),
                model_avg_lin_rescaled = c(NA_real_, NA_real_),
                geom_text_label = c("", "")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df", "tbl", "data.frame")
            ),
            agecat = structure(
              list(
                factor = c("agecat", "agecat", "agecat", "agecat",
                           "agecat", "agecat"),
                orig_level = c("1", "2", "3",
                               "4", "5", "6"),
                actual_level = c("1", "2", "3", "4",
                                 "5", "6"),
                weight = c(2L, 4L, 5L, 3L, 1L, 1L),
                obs_avg_pred_nonrescaled = c(
                  2401.90498899,
                  687.6949995725,
                  269.488000392,
                  3050.96454493333,
                  353.76999998,
                  480
                ),
                obs_avg_lin_nonrescaled = c(
                  7.78401744689924,
                  6.53334542506553,
                  5.59652386391064,
                  8.02321306384722,
                  5.86864698440522,
                  6.17378610390194
                ),
                obs_avg_pred_rescaled = c(
                  8.91284578718223,
                  2.55185759132938,
                  1,
                  11.3213372784516,
                  1.31274861762083,
                  1.78115537352976
                ),
                obs_avg_lin_rescaled = c(
                  2.1874935829886,
                  0.936821561154892,
                  0,
                  2.42668919993658,
                  0.272123120494585,
                  0.577262239991296
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0,
                                            0, 0, 0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_, NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_, NA_real_, NA_real_,
                                           NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "", "")
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            area = structure(
              list(
                factor = c("area",
                           "area", "area", "area", "area", "area"),
                orig_level = c("A",
                               "B", "C", "D", "E", "F"),
                actual_level = c("A", "B",
                                 "C", "D", "E", "F"),
                weight = c(6L, 2L, 6L, 1L, 1L, NA),
                obs_avg_pred_nonrescaled = c(1154.54393560833, 1121.29999899,
                                             1506.47166689667, 480, 200, NA),
                obs_avg_lin_nonrescaled = c(
                  7.05146068403262,
                  7.02224400456609,
                  7.31752555115597,
                  6.17378610390194,
                  5.29831736654804,
                  NA
                ),
                obs_avg_pred_rescaled = c(
                  1,
                  0.971206001267663,
                  1.30481969584198,
                  0.415748578461058,
                  0.173228574358774,
                  NA
                ),
                obs_avg_lin_rescaled = c(
                  0,
                  -0.0292166794665221,
                  0.266064867123355,
                  -0.87767458013068,
                  -1.75314331748458,
                  NA
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  NA
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  NA
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1, NA),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0, NA),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "", "")
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            veh_body = structure(
              list(
                factor = c(
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body",
                  "veh_body"
                ),
                orig_level = c(
                  "BUS",
                  "CONVT",
                  "COUPE",
                  "HBACK",
                  "HDTOP",
                  "MCARA",
                  "MIBUS",
                  "PANVN",
                  "RDSTR",
                  "SEDAN",
                  "STNWG",
                  "TRUCK",
                  "UTE"
                ),
                actual_level = c(
                  "BUS",
                  "CONVT",
                  "COUPE",
                  "HBACK",
                  "HDTOP",
                  "MCARA",
                  "MIBUS",
                  "PANVN",
                  "RDSTR",
                  "SEDAN",
                  "STNWG",
                  "TRUCK",
                  "UTE"
                ),
                weight = c(NA, NA, NA, 5L, NA, NA, NA, NA, NA, 7L,
                           4L, NA, NA),
                obs_avg_pred_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  2596.137995814,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  418.33051947,
                  744.9224994125,
                  NA,
                  NA
                ),
                obs_avg_lin_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  7.86178023350441,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  6.03627183650862,
                  6.61328018533408,
                  NA,
                  NA
                ),
                obs_avg_pred_rescaled = c(
                  NA,
                  NA,
                  NA,
                  6.20594930320444,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  1,
                  1.78070321131787,
                  NA,
                  NA
                ),
                obs_avg_lin_rescaled = c(
                  NA,
                  NA,
                  NA,
                  1.82550839699579,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  0,
                  0.577008348825463,
                  NA,
                  NA
                ),
                fitted_avg_pred_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  1180.54335085709,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  1180.54335085709,
                  1180.54335085709,
                  NA,
                  NA
                ),
                fitted_avg_lin_nonrescaled = c(
                  NA,
                  NA,
                  NA,
                  7.07373007830543,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  7.07373007830543,
                  7.07373007830543,
                  NA,
                  NA
                ),
                fitted_avg_pred_rescaled = c(NA,
                                             NA, NA, 1, NA, NA, NA, NA, NA, 1, 1, NA, NA),
                fitted_avg_lin_rescaled = c(NA,
                                            NA, NA, 0, NA, NA, NA, NA, NA, 0, 0, NA, NA),
                model_avg_pred_nonrescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_lin_nonrescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_pred_rescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                model_avg_lin_rescaled = c(
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_,
                  NA_real_
                ),
                geom_text_label = c("", "",
                                    "", "", "", "", "", "", "", "", "", "", "")
              ),
              row.names = c(NA,-13L),
              class = c("tbl_df", "tbl", "data.frame")
            ),
            veh_age = structure(
              list(
                factor = c("veh_age", "veh_age", "veh_age", "veh_age"),
                orig_level = c("1", "2", "3", "4"),
                actual_level = c("1",
                                 "2", "3", "4"),
                weight = c(6L, 6L, 3L, 1L),
                obs_avg_pred_nonrescaled = c(1560.73666684167,
                                             446.203939275, 1652.73999277, 1888.829998),
                obs_avg_lin_nonrescaled = c(
                  7.35291321111611,
                  6.10077611045565,
                  7.41018979129897,
                  7.54371286768669
                ),
                obs_avg_pred_rescaled = c(1, 0.285893161066015,
                                          1.05894865410865, 1.21021696877428),
                obs_avg_lin_rescaled = c(0,-1.25213710066045, 0.0572765801828661, 0.190799656570587),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0,
                                            0, 0, 0),
                model_avg_pred_nonrescaled = c(NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "")
              ),
              row.names = c(NA,-4L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            ),
            veh_value = structure(
              list(
                factor = c(
                  "veh_value",
                  "veh_value",
                  "veh_value",
                  "veh_value",
                  "veh_value"
                ),
                orig_level = c(
                  "[0,0.9]",
                  "(0.9,1.32]",
                  "(1.32,1.71]",
                  "(1.71,2.44]",
                  "(2.44,34.6]"
                ),
                actual_level = c(
                  "[0,0.9]",
                  "(0.9,1.32]",
                  "(1.32,1.71]",
                  "(1.71,2.44]",
                  "(2.44,34.6]"
                ),
                weight = c(1L, 4L, 2L, 4L, 5L),
                obs_avg_pred_nonrescaled = c(
                  4450.039978,
                  687.6949995725,
                  3988.440000545,
                  485.033408995,
                  354.17199993
                ),
                obs_avg_lin_nonrescaled = c(
                  8.40066835894016,
                  6.53334542506553,
                  8.29115545612534,
                  6.18421777309083,
                  5.86978267064296
                ),
                obs_avg_pred_rescaled = c(
                  12.5646295553559,
                  1.94169781831545,
                  11.2613080687725,
                  1.36948547341649,
                  1
                ),
                obs_avg_lin_rescaled = c(
                  2.53088568829719,
                  0.663562754422568,
                  2.42137278548237,
                  0.314435102447862,
                  0
                ),
                fitted_avg_pred_nonrescaled = c(
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709,
                  1180.54335085709
                ),
                fitted_avg_lin_nonrescaled = c(
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543,
                  7.07373007830543
                ),
                fitted_avg_pred_rescaled = c(1,
                                             1, 1, 1, 1),
                fitted_avg_lin_rescaled = c(0, 0, 0,
                                            0, 0),
                model_avg_pred_nonrescaled = c(NA_real_, NA_real_,
                                               NA_real_, NA_real_, NA_real_),
                model_avg_lin_nonrescaled = c(NA_real_,
                                              NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_pred_rescaled = c(NA_real_,
                                            NA_real_, NA_real_, NA_real_, NA_real_),
                model_avg_lin_rescaled = c(NA_real_,
                                           NA_real_, NA_real_, NA_real_, NA_real_),
                geom_text_label = c("",
                                    "", "", "", "")
              ),
              row.names = c(NA,-5L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          relativities = list(`base_value` = 1180.54337646846),
          train_predictions = c(
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709,
            1180.54335085709
          ),
          test_predictions = NULL,
          cv_predictions = NULL
        ),
        class = "fitted_model"
      ))
    ),
    class = c("modeling", "setup")
  )

  no_print(setup <- do.call(setup, args))

  file.remove("~/insuRglm/tests/sev_gamma_setup.rds")
  file.remove("~/insuRglm/tests/sev_gamma_model.rds")

  expect_equal(setup, correct_result)

})
