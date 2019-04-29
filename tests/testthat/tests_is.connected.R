context("is.connected")

test_that("error checking",{
  expect_error(is.connected("test"),"Error in is.connected: input must be a numeric matrix")
  expect_error(is.connected(c(1,2,3)),"Error in is.connected: input must be a numeric matrix")
  expect_error(is.connected(matrix(1:6,2,3)),"Error in is.connected: input must be a square matrix")
  expect_error(is.connected(matrix(-1,2,2)),"Error in is.connected: input matrix cannot have negative entries")
})

test_that("other tests",{
  g1<-matrix(1,1,1)
  expect_true(is.connected(g1))
  g1<-matrix(c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0),4,4)
  expect_true(is.connected(g1))
  g2<-matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4)
  expect_false(is.connected(g2))  
})