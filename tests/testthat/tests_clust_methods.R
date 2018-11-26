context("clust_methods")

#tests of set and get functions considered unnecessary

test_that("test print.clust",{
  set.seed(101)
  sig<-matrix(.8,5,5)
  diag(sig)<-1
  lents<-50
  dat1<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
  dat2<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
  dat<-rbind(dat1,dat2)
  times<-1:lents
  dat<-cleandat(dat,times,clev=1)$cdat
  coords<-data.frame(Y=rep(0,10),X=1:10)
  method<-"coh.sig.fast"
  res<-clust(dat,times,coords,method,nsurrogs = 100)
  expect_equal(capture_output(print(res)),"clust object:\ntimes, a length 50 numeric vector:\n1 2 3 4 5 ... 46 47 48 49 50 \nNumber of sampling locations: 10 \nmethodspecs:\nmethod=coh.sig.fast; tsrange=0 to Inf; nsurrogs=100; weighted=TRUE; sigthresh=0.95;\nscale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nadj has 88 of 90 off-diagonal entries differing from 0; values range from 0 to 0.990099 \nNumber of splitting steps done: 1 \nNumber of modules in final decomposition: 2 \nModularity values for each step: 4.45802837530728e-17 0.122387747258127 \nThe wmfs slot is: empty\nThe wpmfs slot is: empty")

  res<-addwmfs(res)
  expect_equal(capture_output(print(res)),"clust object:\ntimes, a length 50 numeric vector:\n1 2 3 4 5 ... 46 47 48 49 50 \nNumber of sampling locations: 10 \nmethodspecs:\nmethod=coh.sig.fast; tsrange=0 to Inf; nsurrogs=100; weighted=TRUE; sigthresh=0.95;\nscale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nadj has 88 of 90 off-diagonal entries differing from 0; values range from 0 to 0.990099 \nNumber of splitting steps done: 1 \nNumber of modules in final decomposition: 2 \nModularity values for each step: 4.45802837530728e-17 0.122387747258127 \nThe wmfs slot is: filled\nThe wpmfs slot is: empty")
})

#when summary function written, add tests
