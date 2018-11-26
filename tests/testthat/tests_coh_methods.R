context("coh_methods")

#tests of set and get functions considered unnecessary

test_that("test print.coh",{
  set.seed(201)
  
  times<-1:35
  dat1<-matrix(rnorm(10*length(times)),10,length(times))
  dat1<-cleandat(dat1,times,1)$cdat
  dat2<-matrix(rnorm(10*length(times)),10,length(times))
  dat2<-cleandat(dat2,times,1)$cdat
  obj<-coh(dat1,dat2,times,"powall","fast")
  
  expect_equal(capture_output(print(obj)),"coh object:\ntimes, a length 35 numeric vector:\n1 2 3 4 5 ... 31 32 33 34 35 \nNumber of sampling locations: 10 \ntimescales, a length 41 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 11.5836322719437 12.1628138855409 12.770954579818 13.4095023088089 14.0799774242493 \nnorm, the normalization used: powall \nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nsigmethod, the type of significance testing used: fast \nNumber of surrogates: 1000 \nThe ranks slot is: empty\nTimescale bands tested in bandp slot: none")
  
  obj<-bandtest(obj,c(3,5))
  obj<-bandtest(obj,c(8,12))
  
  expect_equal(capture_output(print(obj)),"coh object:\ntimes, a length 35 numeric vector:\n1 2 3 4 5 ... 31 32 33 34 35 \nNumber of sampling locations: 10 \ntimescales, a length 41 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 11.5836322719437 12.1628138855409 12.770954579818 13.4095023088089 14.0799774242493 \nnorm, the normalization used: powall \nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nsigmethod, the type of significance testing used: fast \nNumber of surrogates: 1000 \nThe ranks slot is: filled\nTimescale bands tested in bandp slot:\n  ts_low_bd ts_hi_bd\n1         3        5\n2         8       12")
})

#when summary function written, add tests