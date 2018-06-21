context("plotmag")

test_that("test the default method, which just throws an error",{
  object<-list("test","this","is")
  expect_error(plotmag(object),"Error in plotmag: method not defined for this class")
})

test_that("test for an actual wt object",{
  #because plotmag.wt does not exist (the wt method is inherited from tts), we are effectively testing the tts method here
  
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
  
  #make a plot and save a pdf so I can check it visually and then get the hash for testing of future reprodicibility
  pres<-plotmag(object=res,filename="Test_plotmagwt")
  #it looked good so I just check the hash
  myfile <- system.file("Test_plotmagwt.pdf")
  #Hash below was obtained by running tests the first time and seeing what hash it reported when the comparison
  #failed the first time. Using digest::digest(file(myfile)) did not work because when I run the above from 
  #the command line, the working directory was whatever it was, whereas when test() is run, the working directory
  #is tests/testthat, which means the hash will differ even if the plot is the same. Alternatively you could make
  #your working directory be tests/testthat when you run the above from the command line, and then you could 
  #probably use digest::digest(file(myfile)) to get the hash the first time.
  expect_known_hash(res,hash="1661d4fb42b6f127552b7bb27793435f")   
})

#test_that("test for an actual wmf object",{
#  #because plotmag.wt does not exist (the wt method is inherited from tts), we are effectively testing the tts method here  
#})

#test_that("test for an actual wpmf object",{
#
#})

