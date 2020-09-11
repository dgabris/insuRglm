context("Setup - Input data")

test_that("input datasets must be of class data.frame", {

  args <- list(
    data_train = as.list(sev_train),
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    tweedie_p = 1.5
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_error(do.call(setup, args, "'target' must be a character scalar"))

})

test_that("target variable must be present in the datasets", {

  args <- list(
    data_train = sev_train,
    target = 'imaginary_target',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_message(
    no_print(setup <- do.call(setup, args)),
    "'weight' was not provided, each record will have the same weight"
  )

  expect_equal(setup$weight, "_weight")
  expect_equal(setup$data_train[[setup$weight]], rep(1, nrow(setup$data_train)))

})

test_that("weight argument must be a scalar", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = c('numclaims', 'numclaims'),
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_error(do.call(setup, args, "'weight' must be a character scalar"))

})

test_that("weight variable must be present in the datasets", {

  args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'imaginary_weight',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_error(do.call(setup, args, "Weight variable not in the dataset"))

})


context("Setup - Offset")

test_that("offset argument must be a scalar", {

  args <- list(
    data_train = freq_train,
    target = 'freq',
    offset = c('exposure', 'exposure'),
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium')
  )

  expect_error(do.call(setup, args, "'offset' must be a character scalar"))

})

test_that("offset variable must be present in the datasets", {

  args <- list(
    data_train = freq_train,
    target = 'freq',
    offset = 'imaginary_offset',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium')
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
    keep_cols = as.factor(c('pol_nbr', 'exposure', 'premium'))
  )

  expect_error(do.call(setup, args), "'keep_cols' must be a character vector")

})

test_that("keep_cols variables must be present in the datasets", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'imaginary_column')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_message(no_print(setup <- do.call(setup, args)), "All the predictors are now coerced to 'factor' class")

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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
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
    tweedie_p = 1.5
  )

  no_print(setup <- do.call(setup, args))

  expect_equal(attr(setup$data_train$test_column, "orig_levels"), c("a", "b", "c", "d"))
  expect_equal(attr(setup$data_test$test_column, "orig_levels"), c("a", "b", "c", "d"))

})


context("Setup - Family")

test_that("family must be one of poisson, gamma or tweedie", {

  args <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gama',
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  expect_error(do.call(setup, args), "'arg' should be one of \"poisson\", \"gamma\", \"tweedie\"")

})

test_that("poisson family treats weight as offset, if offset is not provided", {

  args <- list(
    data_train = freq_train,
    target = 'freq',
    # offset = 'exposure',
    weight = 'exposure',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium')
  )

  expect_message(
    no_print(setup <- do.call(setup, args)),
    "No 'offset' provided for family 'poisson', will treat 'weight' as 'offset'"
  )

  expect_equal(setup$offset, "exposure")
  expect_equal(setup$weight, "_weight")
  expect_equal(setup$data_train[[setup$weight]], rep(1, nrow(setup$data_train)))

})

test_that("tweedie_p is ignored when family is poisson or gamma", {

  args <- list(
    data_train = freq_train,
    target = 'freq',
    offset = 'exposure',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5
  )

  args2 <-   args <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5
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
    keep_cols = c('pol_nbr', 'premium')
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
    tweedie_p = c(1.5, 1.7)
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
    keep_cols = c('pol_nbr', 'premium')
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
    target = 'freq',
    offset = 'exposure',
    family = 'poisson',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5
  )

  args2 <- list(
    data_train = sev_train,
    target = 'sev',
    weight = 'numclaims',
    family = 'gamma',
    keep_cols = c('pol_nbr', 'exposure', 'premium'),
    tweedie_p = 1.5
  )

  args3 <- list(
    data_train = bc_train,
    target = 'bc',
    weight = 'exposure',
    family = 'tweedie',
    keep_cols = c('pol_nbr', 'premium'),
    tweedie_p = 1.5
  )

  no_print(setup1 <- do.call(setup, args1))
  no_print(setup2 <- do.call(setup, args2))
  no_print(setup3 <- do.call(setup, args3))

  expect_equal(setup1$family, poisson(link = "log"))
  expect_equal(setup2$family, Gamma(link = "log"))
  expect_equal(setup3$family, statmod::tweedie(var.power = 1.5, link.power = 0))

})


context("Setup - working example")

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
    keep_cols = c('pol_nbr', 'exposure', 'premium')
  )

  # no_print(setup <- do.call(setup, args))
  # dput(setup)
  #
  # Copy & paste from console, hightlight the copied code
  # 'Code' -> 'Reformat Code' (or CTRL + Shift + A)

  correct_result <-
    structure(
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
        seed = NULL,
        data_train = structure(
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
              c(3L, 4L, 3L, 4L, 1L, 5L, 4L,
                5L, 1L, 4L, 4L, 4L, 2L, 4L, 3L),
              .Label = c("2000", "2001",
                         "2002", "2003", "2004"),
              class = c("simple_factor", "factor"),
              orig_levels = c("2000", "2001", "2002", "2003", "2004"),
              base_level = "2003"
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
              c(1L,
                2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L),
              .Label = c("F",
                         "M"),
              class = c("simple_factor", "factor"),
              orig_levels = c("F",
                              "M"),
              base_level = "F"
            ),
            agecat = structure(
              c(4L, 2L, 3L,
                3L, 4L, 4L, 6L, 2L, 5L, 1L, 1L, 3L, 3L, 2L, 3L),
              .Label = c("1",
                         "2", "3", "4", "5", "6"),
              class = c("simple_factor", "factor"),
              orig_levels = c("1", "2", "3", "4", "5", "6"),
              base_level = "3"
            ),
            area = structure(
              c(5L, 2L, 1L, 3L, 3L, 1L, 4L, 3L, 2L,
                1L, 1L, 3L, 3L, 1L, 1L),
              .Label = c("A", "B", "C", "D",
                         "E", "F"),
              class = c("simple_factor", "factor"),
              orig_levels = c("A",
                              "B", "C", "D", "E", "F"),
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
              c(1L,
                4L, 2L, 2L, 1L, 2L, 1L, 3L, 1L, 3L, 2L, 2L, 1L, 1L, 2L),
              .Label = c("1", "2", "3", "4"),
              class = c("simple_factor",
                        "factor"),
              orig_levels = c("1", "2", "3", "4"),
              base_level = "1"
            ),
            veh_value = structure(
              c(5L, 2L, 4L, 4L, 3L, 4L, 5L, 2L,
                5L, 1L, 2L, 4L, 3L, 5L, 5L),
              .Label = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              class = c("simple_factor",
                        "factor"),
              orig_levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              base_level = "(2.44,34.6]"
            ),
            numclaims = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L,
                          1L, 1L, 1L, 1L, 1L),
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
          row.names = c(NA, -15L),
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        data_test = structure(
          list(
            pol_nbr = c("214771",
                        "489640", "344317", "487354", "382906"),
            pol_yr = structure(
              c(5L,
                1L, 1L, 4L, 4L),
              .Label = c("2000", "2001", "2002", "2003",
                         "2004"),
              class = c("simple_factor", "factor"),
              orig_levels = c("2000",
                              "2001", "2002", "2003", "2004"),
              base_level = "2003"
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
              .Label = c("F",
                         "M"),
              class = c("simple_factor", "factor"),
              orig_levels = c("F",
                              "M"),
              base_level = "F"
            ),
            agecat = structure(
              c(3L, 1L, 3L,
                2L, 4L),
              .Label = c("1", "2", "3", "4", "5", "6"),
              class = c("simple_factor",
                        "factor"),
              orig_levels = c("1", "2", "3", "4", "5", "6"),
              base_level = "3"
            ),
            area = structure(
              c(3L, 2L, 1L, 4L, 3L),
              .Label = c("A",
                         "B", "C", "D", "E", "F"),
              class = c("simple_factor",
                        "factor"),
              orig_levels = c("A", "B", "C", "D", "E", "F"),
              base_level = "A"
            ),
            veh_body = structure(
              c(11L, 4L,
                4L, 11L, 10L),
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
              c(3L,
                1L, 2L, 4L, 3L),
              .Label = c("1", "2", "3", "4"),
              class = c("simple_factor",
                        "factor"),
              orig_levels = c("1", "2", "3", "4"),
              base_level = "1"
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
              class = c("simple_factor", "factor"),
              orig_levels = c(
                "[0,0.9]",
                "(0.9,1.32]",
                "(1.32,1.71]",
                "(1.71,2.44]",
                "(2.44,34.6]"
              ),
              base_level = "(2.44,34.6]"
            ),
            numclaims = c(1L, 1L,
                          1L, 1L, 1L),
            sev = c(200, 3981.2299957, 1265.4499969,
                    2723.0399933, 200)
          ),
          row.names = c(NA, -5L),
          class = c("tbl_df",
                    "tbl", "data.frame")
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
            predictors = NULL
          ),
          class = "unfitted_model"
        )
      ),
      class = "setup"
    )


  no_print(setup <- do.call(setup, args))

  expect_equal(setup, correct_result)

})
