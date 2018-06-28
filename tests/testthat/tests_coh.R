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

#coh<-function(dat1,dat2,times,norm,sigmethod="none",nrand=1000,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
#{


#test_that("",{
#  
#})

#test_that("",{
#  
#})

