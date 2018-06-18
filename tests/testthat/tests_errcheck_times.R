context("errcheck_times")

test_that("",{
  expect_error(errcheck_times("nonnumeric","notrealfunc"),
               "Error in errcheck_times called by notrealfunc: times must be numeric")
  expect_error(errcheck_times(1,"notrealfunc"),
               "Error in errcheck_times called by notrealfunc: times must be a vector")
  expect_error(errcheck_times(c(1,2,3,NA),"notrealfunc"),
               "Error in errcheck_times called by notrealfunc: times must not contain NAs, NaNs, Infs")
  expect_error(errcheck_times(c(1,3,4,5),"notrealfunc"),
               "Error in errcheck_times called by notrealfunc: times must be evenly spaced")
  expect_error(errcheck_times(c(5,4,3,2),"notrealfunc"),
               "Error in errcheck_times called by notrealfunc: times must be increasing")
})