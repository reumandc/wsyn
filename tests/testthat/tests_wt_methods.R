context("wt_methods")

#tests of set and get functions considered unnecessary

test_that("test print.wt", {
  set.seed(101)
  
  times<-1:12
  t.series<-rnorm(length(times))
  t.series<-cleandat(t.series,times,1)$cdat
  obj1<-wt(t.series,times)
  
  times<-1:9
  t.series<-rnorm(length(times))
  t.series<-cleandat(t.series,times,1)$cdat
  obj2<-wt(t.series,times)
  
  times<-1:7
  t.series<-rnorm(length(times))
  t.series<-cleandat(t.series,times,1)$cdat
  obj3<-wt(t.series,times)
  
  expect_equal(capture_output(print(obj1)),"wt object:\ntimes, a length 12 numeric vector:\n1 2 3 4 5 ... 8 9 10 11 12 \ntimescales, a length 16 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 ... 3.42067871623263 3.59171265204426 3.77129828464647 3.9598631988788 4.15785635882274 \nvalues, a 12 by 16 matrix, upper left is:\n              [,1]                  [,2]                  [,3]\n[1,]            NA                    NA                    NA\n[2,]            NA                    NA                    NA\n[3,]            NA                    NA                    NA\n[4,]  1.0229526+0i  0.9999241-0.1560218i  0.9482844-0.2946847i\n[5,] -0.7071928-0i -0.6912389+0.2461984i -0.6678158+0.4657171i\n                      [,4]                  [,5]\n[1,]                    NA                    NA\n[2,]                    NA                    NA\n[3,]                    NA                    NA\n[4,]  0.8710103-0.3772524i  0.7561441-0.3845589i\n[5,] -0.6359387+0.5936624i -0.5821650+0.5952190i\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1")

  expect_equal(capture_output(print(obj2)),"wt object:\ntimes, a length 9 numeric vector:\n1 2 3 4 5 6 7 8 9 \ntimescales, a length 11 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 2.552563125 2.68019128125 2.8142008453125 2.95491088757813 3.10265643195703 3.25778925355488 \nvalues, a 9 by 11 matrix, upper left is:\n               [,1]                  [,2]                  [,3]\n[1,]             NA                    NA                    NA\n[2,]             NA                    NA                    NA\n[3,]             NA                    NA                    NA\n[4,] -0.43098284-0i -0.4199598+0.3874250i -0.2501042+0.7390317i\n[5,] -0.01711724+0i -0.0177849-0.1360741i -0.2142956-0.2576680i\n                      [,4]                  [,5]\n[1,]                    NA                    NA\n[2,]                    NA                    NA\n[3,]                    NA                    NA\n[4,]  0.0410816+1.0375477i  0.3853295+1.2712428i\n[5,] -0.5604716-0.3350345i -0.9703238-0.3517005i\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1")

  expect_equal(capture_output(print(obj3)),"wt object:\ntimes, a length 7 numeric vector:\n1 2 3 4 5 6 7 \ntimescales, a length 5 numeric vector:\n2 2.1 2.205 2.31525 2.4310125 \nvalues, a 7 by 5 matrix, upper left is:\n            [,1]             [,2]              [,3]             [,4]\n[1,]          NA               NA                NA               NA\n[2,]          NA               NA                NA               NA\n[3,]          NA               NA                NA               NA\n[4,] 2.242876+0i 2.1896+0.179423i 2.130585+0.34132i 2.0667+0.463176i\n[5,]          NA               NA                NA               NA\n                   [,5]\n[1,]                 NA\n[2,]                 NA\n[3,]                 NA\n[4,] 1.993106+0.531653i\n[5,]                 NA\nwtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1")
})

#when summary function written, add tests
