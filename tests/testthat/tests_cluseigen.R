context("cluseigen")

test_that("error checking",{
  expect_error(cluseigen("test"),"Error in cluseigen: input must be a numeric matrix")
  expect_error(cluseigen(c(1,2,3)),"Error in cluseigen: input must be a numeric matrix")
  expect_error(cluseigen(matrix(1:6,2,3)),"Error in cluseigen: input must be a square matrix")
  expect_error(cluseigen(matrix(1,1,1)),"Error in cluseigen: input matrix must have dimensions at least 2")  
  expect_error(cluseigen(matrix(c(0,1,2,0),2,2)),"Error in cluseigen: input matrix must be symmetric")
  expect_error(cluseigen(matrix(c(1,1,1,0),2,2)),"Error in cluseigen: diagonal of input matrix must contain only zeros")
})

test_that("test on a completely connected graph",{
  adj<-matrix(1,5,5)
  diag(adj)<-0
  res<-cluseigen(adj)
  expect_equal(class(res),"list")
  expect_equal(length(res),1)
  expect_equal(res[[1]],rep(1,5))
})

test_that("test on a simple disconnected example",{
  adj<-matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4)
  res<-cluseigen(adj)
  expect_equal(class(res),"list")
  expect_equal(length(res),2)
  expect_equal(res[[1]],rep(1,4))
  expect_equal(res[[2]],c(1,1,2,2))
})

test_that("test on a simple example where there should be two splits",{
  adj<-matrix(c(0,1,0,0,0,0,0,0,
                1,0,.5,0,0,0,0,0,
                0,.5,0,1,0,0,0,0,
                0,0,1,0,0,0,0,0,
                0,0,0,0,0,1,0,0,
                0,0,0,0,1,0,.5,0,
                0,0,0,0,0,.5,0,1,
                0,0,0,0,0,0,1,0),8,8,byrow=TRUE)
  res<-cluseigen(adj)
  expect_equal(length(res),4)
  expect_equal(res[[1]],rep(1,8))
  expect_equal(res[[2]],c(rep(2,4),rep(1,4)))
  expect_equal(res[[4]],c(4,4,3,3,2,2,1,1))
})