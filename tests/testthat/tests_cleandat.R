context("cleandat")

test_that("test error catching not covered elsewhere",{
  times<-1:100
  dat<-rnorm(100)
  expect_error(cleandat(dat,times,clev=5),"Error in cleandat: clev must be 1, 2, 2, or 4")
  
  expect_error(cleandat("test",times,clev=4),"Error in cleandat: dat must be numeric")
  
  dat<-rnorm(99)
  expect_error(cleandat(dat,times,clev=1),"Error in cleandat: length of dat and times must be equal")
  
  dat<-c(dat,NA)
  expect_error(cleandat(dat,times,clev=1),"Error in cleandat: dat must not contain NAs, NaNs, Infs")
  
  dat<-2*times+1
  dat<-dat-mean(dat)
  expect_error(cleandat(dat,times,clev=3),"Error in cleandat: cannot perform clev 3 cleaning on time series that are constant or a perfect linear trend")

  times<-1:100
  dat<-rnorm(100)
  clev<-4
  mints<-(-1)
  expect_error(cleandat(dat=dat,times=times,clev=clev,mints=mints),"Error in cleandat: mints, if specified and if clev is 4, must be positive")
})

test_that("clev is 1",{
  times<-1:100
  dat<-rnorm(100)
  clev<-1
  res<-cleandat(dat,times,clev)
  expect_equal(as.vector(res$cdat),dat-mean(dat))
  expect_equal(res$clev,1)
  expect_equal(is.na(res$optlambdas),TRUE)
})

test_that("clev is 2",{
  times<-1:100
  dat<-matrix(rnorm(200),2,100)
  clev<-2
  res<-cleandat(dat,times,clev)
  y<-dat[1,]
  altres<-unname(residuals(lm(y~times)))
  expect_equal(unname(res$cdat[1,]),altres)
})

test_that("clev is 3",{
  times<-1:100
  dat<-matrix(rnorm(200),2,100)
  clev<-3
  res<-cleandat(dat,times,clev)
  y<-dat[1,]
  altres<-unname(residuals(lm(y~times)))
  altres<-altres/sd(altres)
  expect_equal(unname(res$cdat[1,]),altres)
})

test_that("clev is 4",{
  set.seed(101)
  times<-1:100
  dat<-.1*times+rnorm(100,mean=0,sd=.025*times+.5)+1
  plot(times,dat)
  hist(residuals(lm(dat~times)))
  clev<-4
  #res<-cleandat(dat,times,clev)
  
  #test format
  expect_type(res,"list")
  expect_equal(names(res),c("cdat","clev","optlambdas"))
  expect_type(res$optlambdas,"double")
  expect_equal(length(res$optlambdas),1)
  expect_equal(res$clev,4)
  expect_type(res$cdat,"double")
  expect_equal(length(res$cdat),length(times))
  expect_equal(mean(res$cdat),0)
  
  #see if the output is well behaved
  res<-cleandat(dat,times,clev,lambdas=seq(-2,2,.01),mints=NA)
  plot(times,res$cdat)
  coef(lm(res$cdat~times))
  hist(res$cdat)
  res$optlambdas
 
  altres<-boxcox(dat~times)
  altoptlambda<-altres$x[altres$y==max(altres$y)]
  altoptlambda
  altcdat<-bctrans(dat,altoptlambda)
  hist(residuals(lm(altcdat~times)))
  plot(times,altcdat)
})