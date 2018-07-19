#' Community structure detection in networks 
#' 
#' Community structure detection in networks ased on the leading eigenvector of the 
#' community matrix
#' 
#' @param adj An adjacency matrix. Should be symmetric with diagonal containing zeros.
#' 
#' @return \code{cluseigen} returns a list with one element for each of the splits 
#' performed by the clustering algorithm. Each element is a vector with entries 
#' corresponding to rows and columns of adj an indicating the module membership
#' of the node following the split. The last element of the list is the final 
#' clustering determined by algorithm when its halting condition is satisfied.
#' 
#' @author Lei Zhao, \email{lei_journal@yahoo.com}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @details The difference between this function and the function 
#' \code{cluster_leading_eigen} in the \code{igraph} package is that this function 
#' can be used on an adjacency matrix with negative elements, which is very common for 
#' correlation matrices and other measures of pairwise synchrony of time series. If 
#' the matrix is non-negative, the result of this function should be exactly the same 
#' as \code{cluster_leading_eigen}.
#'
#' @references Gomez S., Jensen P. & Arenas A. (2009). Analysis of community structure 
#' in networks of correlated data. Phys Rev E, 80, 016114. 
#' Newman M.E. (2006). Finding community structure in networks using the eigenvectors of 
#' matrices. Phys Rev E, 74, 036104.
#'
#' @examples
#' adj<-matrix(0, 10, 10) # create a fake adjacency matrix
#' adj[lower.tri(adj)]<-runif(10*9/2, -1, 1)
#' adj<-adj+t(adj)
#' colnames(adj)<-letters[1:10]
#' z<-cluseigen(adj)
#' 
#' @export

cluseig<-function(adj)
{
  
  
  
  return(res)
}