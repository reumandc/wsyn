#' Normalization for the \code{coh} function
#' 
#' A convenience function for performing the normalization step for the \code{coh} function.
#' 
#' @param W An array of wavelet transforms, locations by times by timescales
#' @param norm The normalization of wavelet transforms to use. Controls the version of the coherence that is performed. One of "none", "phase", "powall", "powind". See details section of the documentation for \code{coh}.
#' 
#' @return \code{normforcoh} returns an array the same dimensions as W of normalized transforms
#' 
#' @note Internal function, no error checking
#'
#' @author Daniel Reuman, \email{reuman@@ku.edu}
 
normforcoh<-function(W,norm)
{
  if (norm=="none")
  {
    return(W)
  }
  if (norm=="phase")
  {
    return(W/Mod(W))
  }
  if (norm=="powall")
  {
    normdenom<-sqrt(apply(X=(Mod(W))^2,MARGIN=3,FUN=mean,na.rm=T))
    for (i in 1:dim(W)[3])
    {
      W[,,i]<-W[,,i]/normdenom[i]
    }
    return(W)
  }
  if (norm=="powind")
  {
    normdenom<-sqrt(apply(X=(Mod(W))^2,MARGIN=c(1,3),FUN=mean,na.rm=T))
    for (i in 1:dim(W)[2])
    {
      W[,i,]<-W[,i,]/normdenom
    }
    return(W)
  }
}