#' Adds rank information to a \code{coh} object
#' 
#' When a \code{coh} object is created, the \code{ranks} slot is NA. This function fills it in.
#' 
#' @param cohobj An object of class \code{coh}
#' 
#' @return \code{addranks} returns another \code{coh} object with ranks slotted now included.
#' If  \code{cohobj$ranks} was not NA, the \code{cohobj} is returned as is.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking performed
#' 
#' @examples
#' #Not written yet but need some

addranks<-function(cohobj)
{
  if (!any(is.na(cohobj$ranks)))
  {
    return(cohobj)  
  }
  
  x<-Mod(cohobj$signif$coher)
  y<-Mod(cohobj$signif$scoher)

  nr<-nrow(y)
  nc<-ncol(y)
  coher<-NA*numeric(nc)
  scoher<-matrix(NA,nr,nc)
  for (counter in 1:nc)
  {
    coher[counter]<-sum(x[counter]>y[,counter])/nr
    scoher[,counter]<-(rank(y[,counter])-1)/(nr-1)
  }
  
  cohobj$ranks<-list(coher=coher,scoher=scoher)
  
  return(cohobj)
}