context("coh")

test_that("test error catching",{
  dat1<-matrix(1:9,3,3)
  dat2<-matrix(1:12,3,4)
  expect_error(coh(dat1,dat2,1:3,"none"),"Error in coh: dimensions of dat1 and dat2 must agree")

  dat1<-1:4
  dat2<-matrix(1:16,4,4)
  expect_error(coh(dat1,dat2,1:4,"none"),"Error in coh: dimensions of dat1 and dat2 must agree")
  
  dat2<-cleandat(dat2,1:4,1)$cdat
  dat1<-dat2
  expect_error(coh(dat1,dat2,1:4,"test"),"Error in coh: bad value for norm")
  expect_error(coh(dat1,dat2,1:4,"none","test"),"Error in coh: bad value for sigmethod")
  expect_error(coh(dat1,dat2,1:4,"phase","fast"),"Error in coh: no fast significance algorithm for phase coherence")
})

test_that("test for correct format of output",{
  set.seed(101)
  times<-1:100
  dat1<-matrix(rnorm(1000),10,100)
  dat2<-matrix(rnorm(1000),10,100)
  dat1<-cleandat(dat1,times,1)$cdat
  dat2<-cleandat(dat2,times,1)$cdat
  norm<-"powall"
  sigmethod<-"fftsurrog1"
  nrand<-10
  res<-coh(dat1,dat2,times,norm,sigmethod,nrand)
  
  expect_s3_class(res,"coh")
  expect_s3_class(res,"list")
  expect_equal(res$dat1,dat1)
  expect_equal(res$dat2,dat2)
  expect_equal(res$times,times)
  expect_equal(res$sigmethod,sigmethod)
  expect_equal(res$norm,norm)
  
  #other tests that output has the correct format
  expect_equal(is.vector(res$coher),TRUE) 
  expect_equal(is.complex(res$coher),TRUE)
  expect_equal(length(res$coher),length(res$timescales))
  
  expect_equal(is.na(res$bandp),TRUE)
  
  expect_equal(res$coher,res$signif$coher)
  expect_equal(length(res$coher),dim(res$signif$scoher)[2])
  expect_equal(nrand,dim(res$signif$scoher)[1])
  expect_equal(is.complex(res$signif$scoher),TRUE)
})

test_that("compare to a previous coherence example",{
  #this test based on supplementary figure 5 in Sheppard et al, Nature Climate Change, 
  #2016, doi: 10.1038/NCLIMATE2881. 
  
  #make the data
  set.seed(101)
  times<-(-3:100)
  ts1<-sin(2*pi*times/10)
  ts2<-5*sin(2*pi*times/3)
  artsig_x<-matrix(NA,11,length(times)) #the driver
  for (counter in 1:11)
  {
    artsig_x[counter,]=ts1+ts2+rnorm(length(times),mean=0,sd=1.5)
  }
  times<-0:100
  artsig_y<-matrix(NA,11,length(times)) #the driven
  for (counter1 in 1:11)
  {
    for (counter2 in 1:101)
    {
      artsig_y[counter1,counter2]<-mean(artsig_x[counter1,counter2:(counter2+2)])
    }
  }
  artsig_y<-artsig_y+matrix(rnorm(length(times)*11,mean=0,sd=3),11,length(times))
  artsig_x<-artsig_x[,4:104]
  artsig_x<-cleandat(artsig_x,times,1)$cdat
  artsig_y<-cleandat(artsig_y,times,1)$cdat
  
  #call coh
  res<-coh(dat1=artsig_x,dat2=artsig_y,times=times,norm="powall",sigmethod="fftsurrog1",nrand=50,
           f0=0.5,scale.max.input=28)
  
  #Make a plot to check visually. Expected to look like panel panel g of supp fig 5 in the 
  #reference cited above
  #plot(log(1/res$timescales),Mod(res$coher),type="l",xaxt="n",col="red")
  #axis(side=1,at=log(1/c(2,5,10,20)),labels=c(2,5,10,20))
  #qs<-apply(X=Mod(res$signif$scoher),FUN=quantile,MARGIN=2,prob=c(.01,.5,.95,.99))
  #for (counter in 1:dim(qs)[1])
  #{
  #  lines(log(1/res$timescales),qs[counter,])
  #}
  #It looked good so I commented it out, now just check future runs are always the same.
  #Hash below was obtained using digest::digest(res).
  expect_known_hash(res,hash="1135b1a52b2af432f221b6f2dd6489a1")   
})

test_that("test for correct format of output, fast algorithm",{
  set.seed(101)
  times<-1:100
  dat1<-matrix(rnorm(1000),10,100)
  dat2<-matrix(rnorm(1000),10,100)
  dat1<-cleandat(dat1,times,1)$cdat
  dat2<-cleandat(dat2,times,1)$cdat
  norm<-"powall"
  sigmethod<-"fast"
  nrand<-10
  res<-coh(dat1,dat2,times,norm,sigmethod,nrand)
  
  expect_s3_class(res,"coh")
  expect_s3_class(res,"list")
  expect_equal(res$dat1,dat1)
  expect_equal(res$dat2,dat2)
  expect_equal(res$times,times)
  expect_equal(res$sigmethod,sigmethod)
  expect_equal(res$norm,norm)
  
  #other tests that output has the correct format
  expect_equal(is.vector(res$coher),TRUE) 
  expect_equal(is.complex(res$coher),TRUE)
  expect_equal(length(res$coher),length(res$timescales))
  
  expect_equal(is.na(res$bandp),TRUE)
  
  expect_equal(length(res$coher),dim(res$signif$scoher)[2])
  expect_equal(nrand,dim(res$signif$scoher)[1])
  expect_equal(is.complex(res$signif$scoher),TRUE)
})

test_that("compare to a previous coherence example, fast algorrithm",{
  #this test based on supplementary figure 5 in Sheppard et al, Nature Climate Change, 
  #2016, doi: 10.1038/NCLIMATE2881. 
  
  #make the data
  set.seed(101)
  times<-(-3:100)
  ts1<-sin(2*pi*times/10)
  ts2<-5*sin(2*pi*times/3)
  artsig_x<-matrix(NA,11,length(times)) #the driver
  for (counter in 1:11)
  {
    artsig_x[counter,]=ts1+ts2+rnorm(length(times),mean=0,sd=1.5)
  }
  times<-0:100
  artsig_y<-matrix(NA,11,length(times)) #the driven
  for (counter1 in 1:11)
  {
    for (counter2 in 1:101)
    {
      artsig_y[counter1,counter2]<-mean(artsig_x[counter1,counter2:(counter2+2)])
    }
  }
  artsig_y<-artsig_y+matrix(rnorm(length(times)*11,mean=0,sd=3),11,length(times))
  artsig_x<-artsig_x[,4:104]
  artsig_x<-cleandat(artsig_x,times,1)$cdat
  artsig_y<-cleandat(artsig_y,times,1)$cdat
  
  #call coh
  res<-coh(dat1=artsig_x,dat2=artsig_y,times=times,norm="powall",sigmethod="fast",nrand=1000,
           f0=0.5,scale.max.input=28)
  
  #Make a plot to check visually. Expected to look like panel panel g of supp fig 5 in the 
  #reference cited above in some respects 
  qs<-apply(X=Mod(res$signif$scoher),FUN=quantile,MARGIN=2,prob=c(.01,.5,.95,.99))
  plot(log(1/res$timescales),Mod(res$coher),type="l",lty="dashed",xaxt="n",col="red",
       ylim=range(Mod(res$coher),Mod(res$signif$coher),qs))
  axis(side=1,at=log(1/c(2,5,10,20)),labels=c(2,5,10,20))
  lines(log(1/res$timescales),Mod(res$signif$coher),type="l",xaxt="n",col="red")
  for (counter in 1:dim(qs)[1])
  {
    lines(log(1/res$timescales),qs[counter,])
  }
  #It looked good so I commented it out, now just check future runs are always the same.
  #Hash below was obtained using digest::digest(res).
  #expect_known_hash(res,hash="1135b1a52b2af432f221b6f2dd6489a1")   
})

#test_that("test the fast coherence part of the code",{
#  
#})


