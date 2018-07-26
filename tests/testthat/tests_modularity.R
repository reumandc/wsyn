context("modularity")

test_that("error checking",{
  expect_error(modularity("test",1:3),"Error in modularity: adj must be a numeric matrix")
  expect_error(modularity(1:3,1:3),"Error in modularity: adj must be a numeric matrix")
  expect_error(modularity(matrix(1:6,2,3),1:3),"Error in modularity: adj must be a square matrix")
  expect_error(modularity(matrix(1,1,1),1:3),"Error in modularity: adj must have dimensions at least 2")
  expect_error(modularity(matrix(c(0,1,2,0),2,2),1:3),"Error in modularity: adj must be symmetric")
  expect_error(modularity(matrix(c(1,1,1,0),2,2),1:3),"Error in modularity: diagonal of adj must contain only zeros")  
  expect_error(modularity(matrix(c(0,1,1,0),2,2),"test"),"Error in modularity: membership must be a numeric vector")    
  expect_error(modularity(matrix(c(0,1,1,0),2,2),1:3),"Error in modularity: membership must have length equal to the dimension of adj")
  expect_error(modularity(matrix(c(0,1,1,0),2,2),c(1,3)),"Error in modularity: entries of membership must be the first n whole numbers")  
})

test_that("test against the igraph function - only tests the main result, not the decomp",{
  gr<-igraph::make_graph(edges="Bull")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res1<-modularity(ma,membership,decomp=FALSE)
  resi<-igraph::modularity(gr,membership)
  expect_equal(res1,resi)
  
  gr<-igraph::make_graph(edges="Chvatal")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res1<-modularity(ma,membership,decomp=FALSE)
  resi<-igraph::modularity(gr,membership)
  expect_equal(res1,resi)

  gr<-igraph::make_graph(edges="Coxeter")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res1<-modularity(ma,membership,decomp=FALSE)
  resi<-igraph::modularity(gr,membership)
  expect_equal(res1,resi)

  gr<-igraph::make_graph(edges="Cubical")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res1<-modularity(ma,membership,decomp=FALSE)
  resi<-igraph::modularity(gr,membership)
  expect_equal(res1,resi)
})

test_that("tests of the decomposition",{
  #test that the decomps add up to the total
  gr<-igraph::make_graph(edges="Bull")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res<-modularity(ma,membership,decomp=TRUE)
  expect_equal(res$totQ,sum(res$modQ))
  expect_equal(res$totQ,sum(res$nodeQ))
    
  gr<-igraph::make_graph(edges="Chvatal")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res<-modularity(ma,membership,decomp=TRUE)
  expect_equal(res$totQ,sum(res$modQ))
  expect_equal(res$totQ,sum(res$nodeQ))

  gr<-igraph::make_graph(edges="Coxeter")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res<-modularity(ma,membership,decomp=TRUE)
  expect_equal(res$totQ,sum(res$modQ))
  expect_equal(res$totQ,sum(res$nodeQ))

  gr<-igraph::make_graph(edges="Cubical")
  ma<-igraph::as_adj(gr,sparse=FALSE)
  n<-dim(ma)[1]
  membership<-c(rep(1,floor(n/2)),rep(2,ceiling(n/2)))
  res<-modularity(ma,membership,decomp=TRUE)
  expect_equal(res$totQ,sum(res$modQ))
  expect_equal(res$totQ,sum(res$nodeQ))
})


