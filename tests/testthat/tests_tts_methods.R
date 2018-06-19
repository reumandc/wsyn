context("tts_methods")

test_that("test set_dat",{
  times<-1:10
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  dat<-NA
  expect_error(set_dat(obj,dat),
               "Error in set_dat: set_dat not defined for this class")
  
  obj<-tts(times,timescales,values) 
  expect_error(set_dat(obj,dat),"Error in set_dat: set_dat not defined for this class")
})

test_that("test set_times", {
  times<-1:10
  newtimes<-2:11
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  expect_error(set_times(obj,newtimes),
               "Error in set_times: set_times not defined for this class")

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

test_that("test get_dat",{
  times<-1:10
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  expect_error(get_dat(obj),
               "Error in get_dat: get_dat not defined for this class")
  
  obj<-tts(times,timescales,values) 
  expect_error(get_dat(obj),
               "Error in get_dat: get_dat not defined for this class")
})

test_that("test get_timescales", {
  times<-1:10
  timescales<-1/c(1:10)
  values<-matrix(1,length(times),length(timescales))
  obj<-list(times=times,timescales=timescales,values=values)
  expect_error(get_timescales(obj),
               "Error in get_timescales: get_timescales not defined for this class")  

  obj<-tts(times,timescales,values) 
  expect_equal(get_timescales(obj),timescales)  
})

#the other get functions are skipped because they are written in exactly the same way

#need tests for summary and print methods when written
