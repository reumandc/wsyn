context("cluseigen")

test_that("error checking",{
  expect_error(cluseigen("test"),"Error in cluseigen: input must be a numeric matrix")
  expect_error(cluseigen(c(1,2,3)),"Error in cluseigen: input must be a numeric matrix")
  expect_error(cluseigen(matrix(1:6,2,3)),"Error in cluseigen: input must be a square matrix")
  expect_error(cluseigen(matrix(1,1,1)),"Error in cluseigen: input matrix must have dimensions at least 2")  
  expect_error(cluseigen(matrix(c(0,1,2,0),2,2)),"Error in cluseigen: input matrix must be symmetric")
  expect_error(cluseigen(matrix(c(1,1,1,0),2,2)),"Error in cluseigen: diagonal of input matrix must contain only zeros")
})

test_that("test on a completely connected graph",{
  adj<-matrix(1,5,5)
  diag(adj)<-0
  res<-cluseigen(adj)
  expect_equal(class(res),"list")
  expect_equal(length(res),1)
  expect_equal(res[[1]],rep(1,5))
})

test_that("test on a simple disconnected example",{
  adj<-matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4)
  res<-cluseigen(adj)
  expect_equal(class(res),"list")
  expect_equal(length(res),2)
  expect_equal(res[[1]],rep(1,4))
  expect_equal(res[[2]],c(1,1,2,2))
})

test_that("test on a simple example where there should be two splits",{
  adj<-matrix(c(0,1,0,0,0,0,0,0,
                1,0,.5,0,0,0,0,0,
                0,.5,0,1,0,0,0,0,
                0,0,1,0,0,0,0,0,
                0,0,0,0,0,1,0,0,
                0,0,0,0,1,0,.5,0,
                0,0,0,0,0,.5,0,1,
                0,0,0,0,0,0,1,0),8,8,byrow=TRUE)
  res<-cluseigen(adj)
  expect_equal(length(res),4)
  expect_equal(res[[1]],rep(1,8))
  expect_equal(res[[2]],c(rep(2,4),rep(1,4)))
  expect_equal(res[[4]],c(4,4,3,3,2,2,1,1))
})

#test_that("test for accuracy against the igraph function in some cases that function applies",{
#  gr<-igraph::make_graph(edges="Bull")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  expect_equal(res[[length(res)]],ires$membership) #currently there is disagreement
#  modularity(ma,res[[length(res)]]) 
#  modularity(ma,ires$membership) #same modularity
#  
#  gr<-igraph::make_graph(edges="Chvatal")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  h<-res[[length(res)]]
#  hnew<-h
#  hnew[h==1]<-2
#  hnew[h==2]<-1 #reverse edge names, since they are arbitrary anyway
#  expect_equal(hnew,ires$membership) 
#  
#  gr<-igraph::make_graph(edges="Coxeter")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  expect_equal(res[[length(res)]],ires$membership) #currently major disagreement
#  modularity(ma,res[[length(res)]]) #this is higher, so cluseigen is actually doing better
#  modularity(ma,ires$membership)
#  
#  gr<-igraph::make_graph(edges="Cubical")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  expect_equal(res[[length(res)]],ires$membership) 
#  
#  gr<-igraph::make_graph(edges="Dodecahedral")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  expect_equal(res[[length(res)]],ires$membership) #currently major disagreement
#  modularity(ma,res[[length(res)]]) #this is higher, so cluseigen is actually doing better
#  modularity(ma,ires$membership)
#  
#  gr<-igraph::make_graph(edges="Franklin")
#  ma<-igraph::as_adj(gr,sparse=FALSE)
#  res<-cluseigen(ma)
#  ires<-igraph::cluster_leading_eigen(gr)
#  expect_equal(res[[length(res)]],ires$membership) #currently major disagreement
#  modularity(ma,res[[length(res)]]) #this is higher, so cluseigen is actually doing better
#  modularity(ma,ires$membership)
#  
#  #so it could be the cluseigen function that is faulty. So comment this whole
#  #test suite out and instead test cluseigen by hand
#})

test_that("unweighted, by-hand checks",{
  #code written by Lei, tests by Dan

  #simple one
  A<-matrix(c(1,1,1,0,0,
              1,1,1,0,0,
              1,1,1,0,0,
              0,0,0,1,1,
              0,0,0,1,1),5,5)
  diag(A)<-0
  k<-colSums(A)
  m<-sum(A)
  P<-matrix(k,length(k),1) %*% matrix(k,1,length(k))/(2*m)
  B<-A-P
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(A)
  expect_equal(res[[2]],-1*sign(es$vectors[,1])/2+1.5)
  
  #more complex one
  set.seed(302)
  A<-matrix(runif(100),10,10)/2
  A<-A+t(A)
  diag(A)<-0
  A<-round(A)
  k<-colSums(A)
  m<-sum(A)/2
  P<-matrix(k,length(k),1) %*% matrix(k,1,length(k))/(2*m)
  B<-A-P
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(A)
  #res[[2]]
  #sign(es$vectors[,1])
  expect_equal(res[[2]],sign(es$vectors[,1])/2+1.5)

  #check the subsequent splitting
  gp1inds<-which(res[[2]]==1)
  Bg1<-B[gp1inds,gp1inds]
  diag(Bg1)<-diag(Bg1)-rowSums(Bg1)
  es1<-eigen(Bg1,symmetric=TRUE)
  expect_equal(es1$vectors[,1]<0,rep(TRUE,4))
  
  gp2inds<-which(res[[2]]==2)
  Bg2<-B[gp2inds,gp2inds]
  diag(Bg2)<-diag(Bg2)-rowSums(Bg2)
  es2<-eigen(Bg2,symmetric=TRUE)
  expect_equal(sign(es2$vectors[,1]),c(-1,-1,-1,1,1,-1))
  
  #another
  set.seed(202)
  A<-matrix(runif(100),10,10)/2
  A<-A+t(A)
  diag(A)<-0
  A<-round(A)
  k<-colSums(A)
  m<-sum(A)/2
  P<-matrix(k,length(k),1) %*% matrix(k,1,length(k))/(2*m)
  B<-A-P
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(A)
  #res[[2]]
  #sign(es$vectors[,1])
  expect_equal(res[[2]],sign(es$vectors[,1])/2+1.5)  
  
  #do a bigger one
  set.seed(101)
  A<-matrix(runif(2500),50,50)/2
  A<-A+t(A)
  diag(A)<-0
  A<-round(A)
  k<-colSums(A)
  m<-sum(A)/2
  P<-matrix(k,length(k),1) %*% matrix(k,1,length(k))/(2*m)
  B<-A-P
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(A)
  h<-sign(es$vectors[,1])
  #rbind(res[[2]],h)
  h[h==1]<-2
  h[h==-1]<-1
  expect_equal(res[[2]],h)  
  
  #check the subsequent splitting
  gp1inds<-which(res[[2]]==1)
  Bg1<-B[gp1inds,gp1inds]
  diag(Bg1)<-diag(Bg1)-rowSums(Bg1)
  es1<-eigen(Bg1,symmetric=TRUE)
  h<-sign(es1$vectors[,1])
  #rbind(res[[3]][gp1inds],h) #looks like it should have been split but wasnt so check whether the 
  #proposed split actually makes the modularity go down, in which case it would not have been done
  expect_lt(sum(Bg1[h<0,h<0])+sum(Bg1[h>0,h>0]),0)
  
  gp2inds<-which(res[[2]]==2)
  Bg2<-B[gp2inds,gp2inds]
  diag(Bg2)<-diag(Bg2)-rowSums(Bg2)
  es2<-eigen(Bg2,symmetric=TRUE)
  h<-sign(es2$vectors[,1])
  #rbind(res[[3]][gp2inds],h)
  h[h==1]<-2
  h[h==-1]<-3
  expect_equal(h,res[[3]][gp2inds])
  expect_gt(sum(Bg2[h==2,h==2])+sum(Bg2[h==3,h==3]),0) #make sure the proposed split was not making
  #the modularity go down, because if it were it should not have been adopted
})

test_that("positive weights, by-hand checks",{
  set.seed(302)
  A<-matrix(runif(100),10,10)
  A<-A+t(A)
  diag(A)<-0
  k<-colSums(A)
  m<-sum(A)/2
  P<-matrix(k,length(k),1) %*% matrix(k,1,length(k))/(2*m)
  B<-A-P
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(A)
  #res[[2]]
  #sign(es$vectors[,1])
  h<-sign(es$vectors[,1])
  h[h==-1]<-2
  expect_equal(res[[2]],h)  
  
  #check subsequent splits
  gp1inds<-which(res[[2]]==1)
  Bg1<-B[gp1inds,gp1inds]
  diag(Bg1)<-diag(Bg1)-rowSums(Bg1)
  es1<-eigen(Bg1,symmetric=TRUE)
  h<-sign(es1$vectors[,1])
  expect_equal(h,rep(-1,3)) #because gp 1 was not further split
  
  gp2inds<-which(res[[2]]==2)
  Bg2<-B[gp2inds,gp2inds]
  diag(Bg2)<-diag(Bg2)-rowSums(Bg2)
  es2<-eigen(Bg2,symmetric=TRUE)$vectors[,1]
  h<-sign(es2)
  #res[[3]][gp2inds]
  #h
  h[h==-1]<-3
  h[h==1]<-2
  expect_equal(h,res[[3]][gp2inds])
  expect_gt(sum(Bg2[h==2,h==2])+sum(Bg2[h==3,h==3]),0) #make sure the proposed split was not making
  #the modularity go down, because if it were it should not have been adopted
})

test_that("positive and negative weights, by-hand checks",{
  set.seed(101)
  wij<-matrix(rnorm(100),10,10)
  wij<-wij+t(wij)
  diag(wij)<-0
  wijplus<-matrix(pmax(0,wij),10,10)
  wijminus<-matrix((-1)*pmin(0,wij),10,10)
  wiplus<-colSums(wijplus)
  wiminus<-colSums(wijminus)
  wplus<-sum(wijplus)/2
  wminus<-sum(wijminus)/2
  B<-wij-(matrix(wiplus,length(wiplus),1) %*% matrix(wiplus,1,length(wiplus)))/(2*wplus)+
    (matrix(wiminus,length(wiminus),1) %*% matrix(wiminus,1,length(wiminus)))/(2*wminus)
  es<-eigen(B,symmetric=TRUE)
  res<-cluseigen(wij)
  #res[[2]]
  #sign(es$vectors[,1])
  h<-sign(es$vectors[,1])
  h[h==1]<-2
  h[h==-1]<-1
  expect_equal(res[[2]],h)  
  
  #do the next split
  gp1inds<-which(res[[2]]==1)
  Bg1<-B[gp1inds,gp1inds]
  diag(Bg1)<-diag(Bg1)-rowSums(Bg1)
  es1<-eigen(Bg1,symmetric=TRUE)
  h<-sign(es1$vectors[,1])
  #h
  #res[[3]][gp1inds]
  h[h==-1]<-2
  expect_equal(h,res[[3]][gp1inds])
  expect_gt(sum(Bg1[h==1,h==1])+sum(Bg1[h==2,h==2]),0) #make sure the proposed split was not making
  #the modularity go down, because if it were it should not have been adopted
  
  gp2inds<-which(res[[2]]==2)
  Bg2<-B[gp2inds,gp2inds]
  diag(Bg2)<-diag(Bg2)-rowSums(Bg2)
  es2<-eigen(Bg2,symmetric=TRUE)$vectors[,1]
  h<-sign(es2)
  #res[[3]][gp2inds]
  #h #these disagreed so check that the proposed split was going to make the modularity go down
  #and so that was why it was not adopted
  expect_lt(sum(Bg2[h==1,h==1])+sum(Bg2[h==-1,h==-1]),0)
})

