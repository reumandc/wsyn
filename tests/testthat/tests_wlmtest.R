context("wlmtest")

test_that("test error checking",{
  set.seed(101)
  times<-1:30
  dat<-list(v1=matrix(rnorm(300),10,30),v2=matrix(rnorm(300),10,30),v3=matrix(rnorm(300),10,30),
            v4=matrix(rnorm(300),10,30),v5=matrix(rnorm(300),10,30))
  dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
  resp<-1
  pred<-2:3
  norm<-"powall"
  res<-wlm(dat,times,resp,pred,norm)
  drop<-4
  sigmethod<-"fft"
  expect_error(wlmtest(wlmobj=res,drop,sigmethod,nrand=1000),
               "Error in wlmtest: drop must contain names or indices of predictors used in fitting wlm")
  drop<-"v4"
  expect_error(wlmtest(wlmobj=res,drop,sigmethod,nrand=1000),
               "Error in wlmtest: drop must contain names or indices of predictors used in fitting wlm")
  drop<-c(3,3)
  expect_error(wlmtest(wlmobj=res,drop,sigmethod,nrand=1000),
               "Error in wlmtest: drop must not have repeat entries")
  drop<-3
  sigmethod<-"test"
  expect_error(wlmtest(wlmobj=res,drop,sigmethod,nrand=1000),
               "Error in wlmtest: bad value for sigmethod")
})

