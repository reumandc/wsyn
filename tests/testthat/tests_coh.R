context("coh")

test_that("test error catching",{
  dat1<-matrix(1:9,3,3)
  dat2<-matrix(1:12,3,4)
  expect_error(coh(dat1,dat2,1:3,"none"),"Error in coh: dimensions of dat1 and dat2 must agree")

  dat1<-1:4
  dat2<-matrix(1:16,4,4)
  expect_error(coh(dat1,dat2,1:4,"none"),"Error in coh: dimensions of dat1 and dat2 must agree")
  
  dat2<-cleandat(dat2,1:4,1)$cdat
  dat1<-dat2
  expect_error(coh(dat1,dat2,1:4,"test"),"Error in coh: bad value for norm")
  expect_error(coh(dat1,dat2,1:4,"none","test"),"Error in coh: bad value for sigmethod")
  expect_error(coh(dat1,dat2,1:4,"phase","fast"),"Error in coh: no fast significance algorithm for phase coherence")
})

test_that("test for correct class of output, and for correct including of data, times, etc",{
  set.seed(101)
  times<-1:100
  dat1<-matrix(rnorm(1000),10,100)
  dat2<-matrix(rnorm(1000),10,100)
  dat1<-cleandat(dat1,times,1)$cdat
  dat2<-cleandat(dat2,times,1)$cdat
  norm<-"powall"
  sigmethod<-"fftsurrog1"
  nrand<-10
  res<-coh(dat1,dat2,times,norm,sigmethod,nrand)
  
  expect_s3_class(res,"coh")
  expect_s3_class(res,"list")
  expect_equal(res$dat1,dat1)
  expect_equal(res$dat2,dat2)
  expect_equal(res$times,times)
  expect_equal(res$sigmethod,sigmethod)
  expect_equal(res$norm,norm)
  
  #other tests that output has the correct format
  expect_equal(is.vector(res$coher),TRUE) 
  expect_equal(is.complex(res$coher),TRUE)
  expect_equal(length(res$coher),length(res$timescales))
  
  expect_equal(is.na(res$bandp),TRUE)
  
  expect_equal(res$coher,res$signif$coher)
  expect_equal(length(res$coher),dim(res$signif$scoher)[2])
  expect_equal(nrand,dim(res$signif$scoher)[1])
  expect_equal(is.complex(res$signif$scoher),TRUE)
})

#test_that("",{
#  expect_equal()
#})

#coh<-function(dat1,dat2,times,norm,sigmethod="none",nrand=1000,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
#{


#test_that("",{
#  
#})

