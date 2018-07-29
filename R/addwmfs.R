#' Adds wavelet mean field information to a \code{clust} object
#' 
#' When a \code{clust} object is created, the \code{wmfs} slot is NA. This function fills it in.
#' 
#' @param obj An object of class \code{clust}
#' 
#' @return \code{addwmfs} returns another \code{clust} object with \code{wmfs} slot now included. 
#' If  \code{obj$wmfs} was not NA, the object is returned as is.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking performed
#' 
#' @examples
#' #Not written yet but need some

addwmfs<-function(obj)
{
  if (!any(is.na(obj$wmfs)))
  {
    return(obj)  
  }
  
  #***DAN; did not get this part done yet
  wmfs<-list()
  for (counter in 1:length(obj$clusters))
  {
    thiswmfs<-list()
  }
  
  
}