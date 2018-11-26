context("wmf_methods")

#tests of set and get functions considered unnecessary

test_that("test print.wmf", {
  set.seed(101)
  
  times<-1:35
  dat<-matrix(rnorm(10*length(times)),10,length(times))
  dat<-cleandat(dat,times,1)$cdat
  obj1<-wmf(dat,times)
  
  expect_equal(capture_output(print(obj1)),"wmf object:\ntimes, a length 35 numeric vector:\n1 2 3 4 5 ... 31 32 33 34 35 \nNumber of sampling locations: 10 \ntimescales, a length 41 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 11.5836322719437 12.1628138855409 12.770954579818 13.4095023088089 14.0799774242493 \nvalues, a 35 by 41 matrix, upper left is:\n                 [,1]                  [,2]                  [,3]\n[1,]         NaN+NaNi        NaN+      NaNi        NaN+      NaNi\n[2,]         NaN+NaNi        NaN+      NaNi        NaN+      NaNi\n[3,]         NaN+NaNi        NaN+      NaNi        NaN+      NaNi\n[4,]  0.07788241-  0i  0.0762725+0.1036329i  0.0943894+0.1989645i\n[5,] -0.20361809-  0i -0.1994417-0.0425929i -0.2279712-0.0816981i\n                      [,4]                  [,5]\n[1,]        NaN+      NaNi        NaN+      NaNi\n[2,]        NaN+      NaNi        NaN+      NaNi\n[3,]        NaN+      NaNi        NaN+      NaNi\n[4,]  0.1268653+0.2797649i  0.1620964+0.3384150i\n[5,] -0.2800554-0.1122587i -0.3356957-0.1299196i\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1")
})

#when summary function written, add tests
