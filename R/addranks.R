#' Adds rank information to a \code{coh} or \code{wlmtest} object
#' 
#' When a \code{coh} or \code{wlmtets} object is created, the \code{ranks} slot is NA. 
#' This function fills it in.
#' 
#' @param obj An object of class \code{coh} or \code{wlmtest}
#' 
#' @return \code{addranks} returns another \code{coh} or \code{wlmtest} object with ranks 
#' slot now included. If  \code{obj$ranks} was not NA, the object is returned as is.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking performed
#' 
#' @seealso \code{\link{coh}}, \code{\link{wlmtest}}, \code{\link{bandtest}}, \code{browseVignettes("wsyn")}

addranks<-function(obj)
{
  if (!any(is.na(obj$ranks)))
  {
    return(obj)  
  }
  
  x<-Mod(obj$signif$coher)
  y<-Mod(obj$signif$scoher)

  nr<-nrow(y)
  nc<-ncol(y)
  coher<-NA*numeric(nc)
  scoher<-matrix(NA,nr,nc)
  for (counter in 1:nc)
  {
    coher[counter]<-sum(x[counter]>y[,counter])/nr
    scoher[,counter]<-(rank(y[,counter])-1)/(nr-1)
  }
  
  obj$ranks<-list(coher=coher,scoher=scoher)
  
  return(obj)
}