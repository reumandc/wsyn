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
  
  Test_plotmag_wt_1<-function(){plotmag(object=res)}
  expect_doppelganger("Test_plotmag_wt_1",Test_plotmag_wt_1)
  
  #make a plot and save a pdf so I can check it visually and then get the hash for testing of future reprodicibility
  #pres<-plotmag(object=res,filename="Test_plotmagwt1")
  #It looked good so I just check the hash. Here are things I checked visually:
  #1) the colorbar appeared to go from about 0.002166182 to 4.761293271, which was the range of values
  #2) peak at 15 yrs for first half, peak at 8 for second half
  #3) times go from 1 to 200
  #expect_equal(digest::digest("Test_plotmagwt1.pdf",file=TRUE),"16fb95b1bdcbe65381d3e4fdd52ab63c")   
  
  #try without the colorbar
  #pres<-plotmag(object=res,colorbar=FALSE,filename="Test_plotmagwt2")
  #myfile2 <- system.file("Test_plotmagwt2.pdf")
  #expect_known_hash(file(myfile2),"c876ef50585fb7dc25e4142203b8972c")
  
  #use neat=T
  #pres<-plotmag(object=res,neat=TRUE,filename="Test_plotmagwt3")
  #myfile3 <- system.file("Test_plotmagwt3.pdf")
  #expect_known_hash(file(myfile3),"064913ac0fbe346580fc475eebff7e40")
  
  
  #try wider z axis limits
  
  
})

#test_that("test for an actual wmf object",{
#  #because plotmag.wt does not exist (the wt method is inherited from tts), we are effectively testing the tts method here  
#})

#test_that("test for an actual wpmf object",{
#
#})

