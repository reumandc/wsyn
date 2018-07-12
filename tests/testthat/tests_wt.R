context("wt")

test_that("test for correct class of output, and for correct including of data and times",{
  times<-1:100
  t.series<-rnorm(length(times))
  t.series<-t.series-mean(t.series)
  res<-wt(t.series, times)
  expect_s3_class(res,"wt")
  expect_s3_class(res,"tts")
  expect_s3_class(res,"list")
  expect_equal(res$times,times)
  expect_equal(res$dat,t.series)
})

test_that("other tests that output has the correct format",{
  times<-1:100
  t.series<-rnorm(length(times))
  t.series<-t.series-mean(t.series)
  res<-wt(t.series, times)
  expect_equal(dim(res$values)[1],length(times))
  expect_equal(dim(res$values)[2],length(res$timescales))
  expect_equal(class(res$values[50,50]),"complex")
})

#maybe add some tests of the timescale output

test_that("test for qualitatively correct output, test 1",{
  #this test based on the first example in fig S2 of Sheppard et al, "Synchrony is more than its
  #top-down and climatic parts: interacting Moran effects on phytoplankton in British seas"
  set.seed(101)
  time1<-1:100
  time2<-101:200
  ts1p1<-sin(2*pi*time1/15)
  ts1p2<-0*time1
  ts2p1<-0*time2
  ts2p2<-sin(2*pi*time2/8)
  ts1<-ts1p1+ts1p2
  ts2<-ts2p1+ts2p2
  ts<-c(ts1,ts2)
  ra<-rnorm(200,mean=0,sd=0.5)
  t.series<-ts+ra
  t.series<-t.series-mean(t.series)
  times<-c(time1,time2)
  res<-wt(t.series, times)
  #Make a plot to check visually. Expected to have a brightly colored band at timescale
  #15 years for the first half of the time series, and then a band at 8 yrs for the second
  #half
  #image(res$times,res$timescales,Mod(res$values))
  #lines(c(1,200),c(15,15))
  #lines(c(1,200),c(8,8))
  #It looked good so I commented it out, now just check future runs are always the same.
  #Hash below was obtained using digest::digest(res).
  expect_known_hash(res,hash="cbef91d7a9af05df85b570f0e0762c4c")   
})

test_that("test for qualitatively correct output, test 2",{
  #this test based on the second example in fig S2 of Sheppard et al, "Synchrony is more than its
  #top-down and climatic parts: interacting Moran effects on phytoplankton in British seas"
  set.seed(201)
  timeinc<-1 #one sample per year
  startfreq<-0.2 #cycles per year
  endfreq<-0.1 #cycles per year
  times<-1:200
  f<-seq(from=startfreq,by=(endfreq-startfreq)/(length(times)-1),to=endfreq) #frequency for each sample
  phaseinc<-2*pi*cumsum(f*timeinc)
  t.series<-sin(phaseinc)
  t.series<-t.series-mean(t.series)
  res<-wt(t.series, times)
  #Make a plot to check visually. Expected to have a brightly colored band at timescale
  #gradually changing from 5 to 10
  #image(res$times,res$timescales,Mod(res$values))
  #lines(c(1,200),c(5,5))
  #lines(c(1,200),c(10,10))
  #It looked good so I commented it out, now just check future runs are always the same.
  #Hash below was obtained using digest::digest(res).
  expect_known_hash(res,hash="370a09c4f5edf95389fea48b0df15291")   
})
