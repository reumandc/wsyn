context("errcheck_tsdat")

test_that("tests for errcheck_tsdat",{
  times<-1:10
  t.series<-"test"
  callfunc<-"notrealfunc"
  expect_error(errcheck_tsdat(times,t.series,callfunc),
               "Error in errcheck_tsdat called by notrealfunc: t.series not numeric")
  
  t.series<-c(1,2)
  expect_error(errcheck_tsdat(times,t.series,callfunc),
               "Error in errcheck_tsdat called by notrealfunc: times and t.series must be the same length")

  t.series<-1:10
  expect_error(errcheck_tsdat(times,t.series,callfunc),
               "Error in errcheck_tsdat called by notrealfunc: t.series must have zero mean")
})