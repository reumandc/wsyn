#' Wavelet transforms for the coh function
#' 
#' Convenience function for computing wavelet transforms for the coh function.
#' 
#' @param dat A locations (rows) x time (columns) matrix (for spatial coherence), or a single time series as a vector
#' @param times The times at which measurements were made
#' @param scale.min The smallest scale of fluctuation that will be examined
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelop
#' 
#' @return \code{wtforcoh} returns an object of class \code{list} with elements:
#' \item{W}{Array of wavelet transforms of \code{dat}, \code{dim(dat)[1]} by times by timescales}
#' \item{timescales}{Vector of timescales for these transforms}
#' 
#' @note Internal function, no error checking
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

wtforcoh<-function(dat,times,scale.min,scale.max.input,sigma,f0)
{
  if (dim(dat)[1]>1)
  {
    W<-warray(dat,times,scale.min,scale.max.input,sigma,f0)
    timescales<-W$timescales
    W<-W$wavarray
  }else
  {
    W<-wt(as.vector(dat1),times,scale.min,scale.max.input,sigma,f0)
    timescales<-get_timescales(W1)
    W1<-get_values(W1)
    W1<-array(W1,c(1,dim(W1)))
    W2<-get_values(wt(as.vector(dat1),times,scale.min,scale.max.input,sigma,f0))
    W2<-array(W2,c(1,dim(W2)))
  }
  return(list(W1=W1,W2=W2,timescales=timescales))
}