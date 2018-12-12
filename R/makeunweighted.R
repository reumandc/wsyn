#' For converting certain synchrony matrices to unweighted versions
#' 
#' Convenience function for converting certain synchrony matrices to unweighted versions
#' 
#' @param mat A synchrony matrix based on significance testing
#' @param sigthresh Significance threshold to use
#' 
#' @return \code{makeunweighted} converts to an unweighted version of the input. Entries of
#' \code{mat} less than \code{sigthresh} become a 1, other entries become a 0. The diagonal
#' is \code{NA}.
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}, Daniel Reuman \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking

makeunweighted<-function(mat,sigthresh)
{
  nlocs<-dim(mat)[1]
  newmat<-matrix(0,nlocs,nlocs)
  diag(mat)<-0
  newmat[mat<sigthresh]<-1
  newmat[mat>=sigthresh]<-0
  diag(newmat)<-NA
  return(newmat)
}