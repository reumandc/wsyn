#' Modularity of a community structure of a graph
#' 
#' Computes the modularity of partitioning of a graph into sub-graphs. Same as the
#' \code{modularity} function in the \code{igraph} package, but allows negative
#' edge weights.
#' 
#' @param adj An adjacency matrix, which should be symmetric with zeros on the diagonal.
#' @param membership Vector of length equal to the number of graph nodes (columns/rows 
#' of \code{adj}) indicating the cluster/sub-graph each nodes belongs to. 
#' @param decomp Logical. If \code{TRUE}, calculate the decomposition of modularity
#' by modules and nodes. Default \code{FALSE}. 
#' 
#' @return \code{modularity} returns a list containing the following:
#' \item{totQ}{The total modularity. This is the only output if \code{decomp=FALSE}}
#' \item{modQ}{The contribution of each module to the total modularity}
#' \item{nodeQ}{The contribution of each node to the total modularity}
#' \item{nodeQrs}{The output \code{nodeQ} rescaled to span (0,1) to give the relative 
#' contribution of each node to the total modularity}
#' 
#' @details The difference between this function and the function \code{modularity} 
#' in the package \code{igraph} is that this function can be used with an adjacency 
#' matrix with negative elements. This is a common case for matrices arrising from a 
#' for correlation matrix or another synchrony matrix. If the matrix is non-negative, 
#' the result of this function should be exactly the same as the result from 
#' \code{modularity} in the \code{igraph} package.
#'
#' @note Adapted from code developed by Robert J. Fletcher, Jr.
#' 
#' @author Jonathan Walter, \email{jonathan.walter@@ku.edu}; Lei Zhao, 
#' \email{lei_journal@@yahoo.com}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references 
#' Fletcher Jr., R.J., et al. (2013) Network modularity reve Reveals critical 
#' scales for connectivity in ecology and evolution. Nature Communications. doi: 10.1038//ncomms3572.
#' Gomez S., Jensen P. & Arenas A. (2009). Analysis of community structure in networks 
#' of correlated data. Phys Rev E, 80, 016114.
#' Newman M.E. (2006). Finding community structure in networks using the eigenvectors 
#' of matrices. Phys Rev E, 74, 036104.
#'
#' @examples
#' adj<-matrix(0, 10, 10) # create a fake adjacency matrix
#' adj[lower.tri(adj)]<-runif(10*9/2, -1, 1)
#' adj<-adj+t(adj)
#' colnames(adj)<-letters[1:10]
#' m<-cluseigen(adj)
#' z<-modularity(adj, m[[length(m)]], decomp=T)
#' 
#' @export

#***DAN: what happens if the membership vector is weird, i.e., not just the first n integers
#where n is the number of modules? Does it cause a bug? In that case, how do the entries of
#modQ correspond to modules?

modularity<-function(adj,membership,decomp=F)
{
  #***DAN: got this far, no further. The below is Lei/Jon's old code, which is 
  #generally good but probably won't work in the modified context of wsyn and needs
  #to be adapted
  
  if(!is.matrix(adj)){stop("The input must be a matrix")}
  if(!isSymmetric(unname(adj))){stop("The input matrix must be symmetric")}
  
  n<-nrow(adj)
  A0<-adj
  k<-colSums(A0)
  m<-sum(k)/2
  
  n.m<-length(unique(membership))
  
  delta<-matrix(0, n, n)
  for(i in 1:n.m){
    tmp<-which(membership==i)
    delta[tmp,tmp]<-1
  }
  
  A0.pos<-A0; A0.pos[A0.pos<0]=0
  A0.neg<-A0; A0.neg[A0.neg>0]=0
  A0.neg<-(-A0.neg)
  
  k.pos<-colSums(A0.pos)
  m.pos<-sum(k.pos)/2
  k.neg<-colSums(A0.neg)
  m.neg<-sum(k.neg)/2
  
  if(m.pos==0){x1<-0 }else{ x1<-k.pos%o%k.pos/2/m.pos}
  if(m.neg==0){x2<-0 }else{ x2<-k.neg%o%k.neg/2/m.neg}
  Q<-(A0-x1+x2)*delta
  
  if(decomp==F){return(sum(Q)/2/(m.pos+m.neg))}else{
    Q.decomp.mod<-rep(NA, n.m)
    for(i in 1:n.m){
      tmp<-which(membership==i)
      Q.decomp.mod[i]<-sum(Q[tmp,tmp])/2/(m.pos+m.neg)
    }
    Q.decomp.node<-(rowSums(Q)+colSums(Q))/4/(m.pos+m.neg)
    Q.decomp.node.rescale<-(Q.decomp.node-min(Q.decomp.node))/diff(range(Q.decomp.node))
    return(list(totQ=sum(Q)/2/(m.pos+m.neg), modQ=Q.decomp.mod, 
                nodeQ=Q.decomp.node, nodeQ.rescale=Q.decomp.node.rescale))
  }
  
}
