context("surrog")

test_that("test Fourier surrogates, synchrony preserving surrogs",{
  surrtype<-"fft"
  syncpres<-TRUE
  
  #***single-ts data

  times<-1:100
  dat<-sin(2*pi*times/10)
  nsurrogs<-10
  res<-surrog(dat,nsurrogs,surrtype,syncpres)
  
  #test format of the output 
  expect_type(res,"list")
  expect_equal(length(res),nsurrogs)
  expect_equal(sapply(FUN=length,X=res),rep(length(times),nsurrogs))
  
  #make sure surrogates have the same Mod(fft) as the real data
  expect_equal(Mod(fft(res[[1]])),Mod(fft(dat)))
  expect_equal(Mod(fft(res[[2]])),Mod(fft(dat)))
  expect_equal(Mod(fft(res[[3]])),Mod(fft(dat)))
  
  #***the same ts repeated twice, to test the same phase randomization is being used
  dat<-rbind(dat,dat)
  res<-surrog(dat,nsurrogs,surrtype,syncpres)

  #test format of the output
  expect_type(res,"list")
  expect_equal(length(res),nsurrogs)
  expect_equal(sapply(FUN=dim,X=res),rbind(rep(2,nsurrogs),rep(length(times),nsurrogs)))
  
  #make sure you get pairs of the same ts for each surrog
  expect_equal(res[[1]][1,],res[[1]][2,])
  expect_equal(res[[2]][1,],res[[2]][2,])
  expect_equal(res[[3]][1,],res[[3]][2,])
  
  #make sure surrogates have the same Mod(fft) as the real data
  expect_equal(Mod(fft(res[[1]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[2]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[3]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[1]][2,])),Mod(fft(dat[2,])))
  expect_equal(Mod(fft(res[[2]][2,])),Mod(fft(dat[2,])))
  expect_equal(Mod(fft(res[[3]][2,])),Mod(fft(dat[2,])))
  
  #***some different time series
  x1<-sin(2*pi*times/10)+rnorm(length(time),0,.1)
  x1<-x1-mean(x1)
  x2<-sin(2*pi*times/20)+rnorm(length(time),0,.1)
  x2<-x2-mean(x2)
  x3<-sin(2*pi*times/20)+rnorm(length(time),0,.1)
  x3<-x3-mean(x3)
  dat<-rbind(x1,x2,x3)
  res<-surrog(dat,nsurrogs,surrtype,syncpres)
  
  #test format of output
  expect_type(res,"list")
  expect_equal(length(res),nsurrogs)
  expect_equal(sapply(FUN=dim,X=res),rbind(rep(3,nsurrogs),rep(length(times),nsurrogs)))
  
  #make sure surrogates have the same Mod(fft) as the real data
  expect_equal(Mod(fft(res[[1]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[1]][2,])),Mod(fft(dat[2,])))
  expect_equal(Mod(fft(res[[1]][3,])),Mod(fft(dat[3,])))
})

test_that("test Fourier surrogates, independent surrogs",{
  surrtype<-"fft"
  syncpres<-FALSE

  #***single-ts data
  set.seed(101)
  times<-1:100
  dat<-sin(2*pi*times/8)
  dat<-dat-mean(dat)
  nsurrogs<-10
  res<-surrog(dat,nsurrogs,surrtype,syncpres)
  
  #test format of the output 
  expect_type(res,"list")
  expect_equal(length(res),nsurrogs)
  expect_equal(sapply(FUN=length,X=res),rep(length(times),nsurrogs))
  
  #make sure surrogates have the same Mod(fft) as the real data
  expect_equal(Mod(fft(res[[1]])),Mod(fft(dat)))
  expect_equal(Mod(fft(res[[2]])),Mod(fft(dat)))
  expect_equal(Mod(fft(res[[3]])),Mod(fft(dat)))
  
  #***the same ts repeated twice, to test different phase randomizations are being used
  dat<-rbind(dat,dat)
  res<-surrog(dat,nsurrogs,surrtype,syncpres)
  
  #test format of the output
  expect_type(res,"list")
  expect_equal(length(res),nsurrogs)
  expect_equal(sapply(FUN=dim,X=res),rbind(rep(2,nsurrogs),rep(length(times),nsurrogs)))
  
  #make sure you you do NOT get the same ts twice for each surrog
  expect_equal(all(res[[1]][1,]==res[[1]][2,]),FALSE)
  expect_equal(all(res[[2]][1,]==res[[2]][2,]),FALSE)
  expect_equal(all(res[[3]][1,]==res[[3]][2,]),FALSE)
  
  #make sure surrogates have the same Mod(fft) as the real data
  expect_equal(Mod(fft(res[[1]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[2]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[3]][1,])),Mod(fft(dat[1,])))
  expect_equal(Mod(fft(res[[1]][2,])),Mod(fft(dat[2,])))
  expect_equal(Mod(fft(res[[2]][2,])),Mod(fft(dat[2,])))
  expect_equal(Mod(fft(res[[3]][2,])),Mod(fft(dat[2,])))
  
  #***test correlations are small for broadband data 
  set.seed(101)
  dat<-matrix(rnorm(20000),2,10000)
  dat[1,]<-dat[1,]-mean(dat[1,])
  dat[2,]<-dat[2,]-mean(dat[2,])
  res<-surrog(dat,nsurrogs=50,surrtype,syncpres)
  getcors<-c()
  for (counter in 1:50)
  {
    getcors[counter]<-cor(res[[counter]][1,],res[[counter]][2,])
  }
  expect_lt(max(abs(getcors)),.05)
})

#test_that("test aaft surrogates, synchrony preserving surrogs",{
#  #to be done together with writing that part of the code in surrog.R  
#  surrtype<-"aaft"
#  syncpres<-TRUE
#})

#test_that("test aaft surrogates, independent surrogs",{
#  #to be done together with writing that part of the code in surrog.R  
#  surrtype<-"aaft"
#  syncpres<-FALSE
#})
