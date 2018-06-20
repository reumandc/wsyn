context("boxcoxloglike")

test_that("just run it to see if it produces something of the right form",{
  times<-1:100
  dat<-.2*times+1+rnorm(100,0,.3)
  res<-boxcoxloglike(1,dat)
  expect_type(res,"double")
  expect_equal(length(res),1)
})

#this function is pretty simple, does not seem necessary to test further

