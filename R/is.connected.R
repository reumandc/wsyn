#' Tests if a graph is connected
#' 
#' Tests if a graph represented by an adjacency matrix is connected. 
#' 
#' @param adj An adjacency matrix. Must be a numeric matrix with non-negative entries.
#' 
#' @return \code{is.connected} returns \code{TRUE} or \code{FALSE} depending on whether
#' the graph represented in \code{adj} is a connected graph.
#' 
#' @details Idea by Ed Scheinerman, circa 2006. 
#' Source: http://www.ams.jhu.edu/~ers/matgraph/; routine: 
#' matgraph/@graph/isconnected.m
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}
#' 
#' @seealso \code{\link{cluseigen}}, \code{\link{clust}}, \code{browseVignettes("wsyn")}
#' 
#' @examples 
#' g1<-matrix(c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0),4,4)
#' is.connected(g1)
#' g2<-matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4)
#' is.connected(g2)
#' 
#' @export

is.connected<-function(adj)
{
  #error checking
  if (!is.numeric(adj))
  {
    stop("Error in is.connected: input must be a numeric matrix")
  }
  if (!is.matrix(adj))
  {
    stop("Error in is.connected: input must be a numeric matrix")
  }
  if (dim(adj)[1]!=dim(adj)[2])
  {
    stop("Error in is.connected: input must be a square matrix")
  }
  if (dim(adj)[1]<2)
  {
    stop("Error in is.connected: input matrix must have dimensions at least 2")
  }
  if (any(adj<0))
  {
    stop("Error in is.connected: input matrix cannot have negative entries")
  }
  
  #now do the algorithm
  if(length(which(colSums(adj)==0))>0)
  { #check for isolated nodes
    return(FALSE)
  }else
  { 
    x<-c(1,rep(0, nrow(adj)-1))
    while(1)
    { #evolve x until a steady state
      y<-x
      x<-adj%*%x + x
      x1<-rep(0,length(x))
      x1[x>0]<-1
      x<-x1
      if(all(x==y)){break}
    }
    if(sum(x)<length(x))
    {
      return(FALSE)
    }else
    {
      return(TRUE)
    }
  } 
}