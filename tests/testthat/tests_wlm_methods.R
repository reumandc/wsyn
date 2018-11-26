context("wlm_methods")

#tests of set and get functions considered unnecessary

test_that("test print.wlm",{
  set.seed(201)
  times<-1:30
  dat<-list(resp=matrix(rnorm(300),10,30),pred1=matrix(rnorm(300),10,30),
            pred2=matrix(rnorm(300),10,30))
  dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
  resp<-1
  pred<-2:3
  norm<-"powall"
  res<-wlm(dat,times,resp,pred,norm)
  
  expect_equal(capture_output(print(res)),"wlm object:\ntimes, a length 30 numeric vector:\n1 2 3 4 5 ... 26 27 28 29 30 \nNumber of sampling locations: 10 \ntimescales, a length 37 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 9.52988293720722 10.0063770840676 10.506695938271 11.0320307351845 11.5836322719437 \nThe wavelet regression: resp~pred1+pred2 \nnorm, the normalization used: powall \nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1")  
})

#when summary function written, add tests
