#' Community structure detection in networks 
#' 
#' Community structure detection in networks based on the leading eigenvector of the 
#' community matrix
#' 
#' @param adj An adjacency matrix. Should be symmetric with diagonal containing zeros.
#' 
#' @return \code{cluseigen} returns a list with one element for each of the splits 
#' performed by the clustering algorithm. Each element is a vector with entries 
#' corresponding to rows and columns of adj and indicating the module membership
#' of the node, following the split. The last element of the list is the final 
#' clustering determined by the algorithm when its halting condition is satisfied.
#' The first element is always a vector of all 1s (corresponding to before any 
#' splits are performed).
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @details The difference between this function and the algorithm described
#' by Newman is that this function can be used on an adjacency matrix with 
#' negative elements, which is very common for correlation matrices and other 
#' measures of pairwise synchrony of time series. 
#'
#' @references Gomez S., Jensen P. & Arenas A. (2009). Analysis of community structure 
#' in networks of correlated data. Phys Rev E, 80, 016114. 
#' 
#' Newman M.E.J. (2006). Finding community structure in networks using the eigenvectors of 
#' matrices. Phys Rev E, 74, 036104.
#' 
#' Newman M.E.J. (2006) Modularity and community structure in networks. PNAS 103, 8577-8582.
#' 
#' @seealso \code{\link{clust}}, \code{\link{modularity}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' adj<-matrix(0, 10, 10) # create a fake adjacency matrix
#' adj[lower.tri(adj)]<-runif(10*9/2, -1, 1)
#' adj<-adj+t(adj)
#' colnames(adj)<-letters[1:10]
#' z<-cluseigen(adj)
#' 
#' @export

cluseigen<-function(adj)
{
  #error checking
  if (!is.numeric(adj))
  {
    stop("Error in cluseigen: input must be a numeric matrix")
  }
  if (!is.matrix(adj))
  {
    stop("Error in cluseigen: input must be a numeric matrix")
  }
  if (dim(adj)[1]!=dim(adj)[2])
  {
    stop("Error in cluseigen: input must be a square matrix")
  }
  if (dim(adj)[1]<2)
  {
    stop("Error in cluseigen: input matrix must have dimensions at least 2")
  }
  if(!isSymmetric(unname(adj)))
  {
    stop("Error in cluseigen: input matrix must be symmetric")
  }
  if(any(diag(adj)!=0))
  {
    stop("Error in cluseigen: diagonal of input matrix must contain only zeros")
  }
  
  #now do the algorithm
  A0<-adj
  n<-nrow(A0)
  
  A0.pos<-A0; A0.pos[A0.pos<0]<-0
  A0.neg<-A0; A0.neg[A0.neg>0]<-0
  A0.neg<-(-A0.neg)
  
  k.pos<-colSums(A0.pos)
  m.pos<-sum(k.pos)/2
  k.neg<-colSums(A0.neg)
  m.neg<-sum(k.neg)/2
  
  if(m.pos==0){tmp1<-0}else{tmp1<-k.pos %o% k.pos/2/m.pos}  
  if(m.neg==0){tmp2<-0}else{tmp2<-k.neg %o% k.neg/2/m.neg}  
  B0<- A0 - tmp1 + tmp2  
  
  modules<-rep(1, n)
  Queue<-vector("list", n)    #record the temporal divisions
  Queue[[1]]<-modules
  a<-1
  r<-2   # index of loops
  
  #main function
  while(a<=max(modules)){  # while there is always a divisible subgraph
    
    # compute modularity matrix
    i.remain<-which(modules==a) # nodes in current (sub)graph to partition
    n1<-length(i.remain)
    
    #if a single node, terminate and check the next module
    if (n1==1)
    {
      a<-a+1
      next
    }
    
    A1<-A0[i.remain,i.remain]
    
    B<-B0[i.remain,i.remain]  #first part of Eq. 51 in Newman 2006
    diag(B)<-diag(B)-colSums(B)  #minus second part of Eq. 51 in Newman 2006
    
    E<-eigen(B,symmetric=TRUE)
    
    #if indivisible, terminate and check the next queue
    if(max(E$values)<=1e-5){
      a<-a+1
      next
    } 
    
    #if delta_Q < 0, terminate and check the next one
    i.max<-which.max(E$values)
    v1<-E$vectors[,i.max]
    i.pos<-which(v1>0)
    i.neg<-which(v1<0)
    if(sum(B[i.pos,i.pos])+sum(B[i.neg,i.neg])<=0){
      a<-a+1
      next
    }
    
    A1[A1>0]<-1
    A1[A1<0]<-0
    if(!is.connected(A1[i.pos,i.pos,drop=FALSE]) | !is.connected(A1[i.neg,i.neg,drop=FALSE])){
      a<-a+1
      next
    }
    modules[modules>=a]<-modules[modules>=a]+1
    modules[i.remain[i.pos]]<-modules[i.remain[i.pos]]-1
    Queue[[r]]<-modules
    r<-r+1
  }
  Queue<-Queue[1:(r-1)]
  
  return(Queue)
}