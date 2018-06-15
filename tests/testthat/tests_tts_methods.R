context("tts_methods")

test_that("test set_times.default", {
  times<-1:10
  newtimes<-2:11
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  expect_error(set_times(obj,newtimes),
               "Error in set_times: set_times only defined for class tts")
})

test_that("test set_times.tts", {
  times<-1:10
  newtimes<-2:11
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-tts(times,timescales,values) 
  h<-set_times(obj,newtimes)
  expect_equal(h$times,newtimes)
  expect_equal(h$timescales,timescales)
  expect_equal(h$values,values)
})

#the other set methods are skipped because they are written in exactly the same way

test_that("test get_timescales.default", {
  times<-1:10
  newtimes<-2:11
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  expect_error(get_timescales(obj),
               "Error in get_timescales: get_timescales only defined for class tts")  
})

test_that("test get_timescales.tts", {
  times<-1:10
  newtimes<-2:11
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-tts(times,timescales,values) 
  expect_equal(get_timescales(obj),timescales)  
})

#the other get functions are skipped because they are written in exactly the same way

#need tests of summary and print methods once those are written