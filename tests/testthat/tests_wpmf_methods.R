context("wpmf_methods")

#tests of set and get functions considered unnecessary

test_that("test print.wpmf", {
  set.seed(101)
  
  times<-1:35
  dat<-matrix(rnorm(10*length(times)),10,length(times))
  dat<-cleandat(dat,times,1)$cdat
  obj1<-wpmf(dat,times)
  obj2<-wpmf(dat,times,sigmethod="quick")
  
  expect_equal(capture_output(print(obj1)),"wpmf object:\ntimes, a length 35 numeric vector:\n1 2 3 4 5 ... 31 32 33 34 35 \nNumber of sampling locations: 10 \ntimescales, a length 41 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 11.5836322719437 12.1628138855409 12.770954579818 13.4095023088089 14.0799774242493 \nvalues, a 35 by 41 matrix, upper left is:\n                     [,1]                  [,2]                  [,3]\n[1,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[2,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[3,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[4,] -2e-01-0.000000e+00i -0.1305735+0.1425041i -0.0706824+0.1846758i\n[5,]  0e+00-1.235979e-14i -0.0346267-0.1169267i -0.0870901-0.1309446i\n                      [,4]                  [,5]\n[1,]        NaN+      NaNi        NaN+      NaNi\n[2,]        NaN+      NaNi        NaN+      NaNi\n[3,]        NaN+      NaNi        NaN+      NaNi\n[4,] -0.0066375+0.1951799i  0.0573871+0.2062358i\n[5,] -0.1257587-0.1167023i -0.1598855-0.1040173i\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nsignificance testing: NA")

  expect_equal(capture_output(print(obj2)),"wpmf object:\ntimes, a length 35 numeric vector:\n1 2 3 4 5 ... 31 32 33 34 35 \nNumber of sampling locations: 10 \ntimescales, a length 41 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 11.5836322719437 12.1628138855409 12.770954579818 13.4095023088089 14.0799774242493 \nvalues, a 35 by 41 matrix, upper left is:\n                     [,1]                  [,2]                  [,3]\n[1,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[2,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[3,]    NaN+         NaNi        NaN+      NaNi        NaN+      NaNi\n[4,] -2e-01-0.000000e+00i -0.1305735+0.1425041i -0.0706824+0.1846758i\n[5,]  0e+00-1.235979e-14i -0.0346267-0.1169267i -0.0870901-0.1309446i\n                      [,4]                  [,5]\n[1,]        NaN+      NaNi        NaN+      NaNi\n[2,]        NaN+      NaNi        NaN+      NaNi\n[3,]        NaN+      NaNi        NaN+      NaNi\n[4,] -0.0066375+0.1951799i  0.0573871+0.2062358i\n[5,] -0.1257587-0.1167023i -0.1598855-0.1040173i\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1\nsignificance testing: quick")
})

#when summary method written, add tests