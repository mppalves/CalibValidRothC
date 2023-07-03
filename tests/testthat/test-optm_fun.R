# devtools::load_all("package/CalibValidRothC")
# source("package/CalibValidRothC/tests/testthat/helper-optm_fun.R")
# debug(optm_fun)

test_that("optm_fun with bad rows yields a warning _and_ a numeric output", {
  suppressWarnings(library(AgreenaRothC2))
  # test data comes from corresponding helper-optm_fun.R
  expect_warning(optm_fun(params = params, train_data = x_train))
  res <- optm_fun(params = params, train_data = x_train)
  expect_equal(res, 18.89721, tolerance = 1e-4)
})
