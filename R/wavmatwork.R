#' Facilitates the computations in synmat for coherence and ReXWT methods
#' 
#' Worker/utility function serving the analysis carried out in synmat for methods based
#' on coherence or real part of the cross-wavelet transform.
#' 
#' @param dat A locations (rows) x time (columns) matrix of measurements
#' @param times The times at which measurements were made, spacing 1
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2. Used 
#' only for wavelet-based methods.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined. Only used 
#' for wavelet-based methods.
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be 
#' greater than 1. Only used for wavelet-based methods.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope. Only used for 
#' wavelet-based methods.
#' @param norm The normalization of wavelet transforms to be used. One of "none", "phase", 
#' "powind".
#' @param treatment Either "Mod" or "Re"
#' 
#' @return \code{wavmatwork} returns a list consisting of:
#' \item{timescales}{The timescales of analysis} 
#' \item{wavarray}{An array, locations by locations by timescales, containing either the 
#' coherences (for \code{treatment="Mod"}) or the real parts of the cross-wavelet transforms
#' (for \code{treatment="Re"}) between locations.}
#' 
#' @note Internal function, no error checking done.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

wavmatwork<-function(dat,times,scale.min,scale.max.input,sigma,f0,norm,treatment)
{
  #basic setup
  nlocs<-dim(dat)[1]
  
  #prepare wavelet transforms and get timescales
  wts<-warray(dat,times,scale.min,scale.max.input,sigma,f0)
  timescales<-wts$timescales
  wts<-wts$wavarray
  wts<-normforcoh(wts,norm)
  
  #get the array output
  wavarray<-array(complex(real=NA,imaginary=NA),c(nlocs,nlocs,length(timescales)))
  for (i in 2:nlocs)
  {
    for (j in 1:(i-1))
    {
      wavarray[i,j,]<-colMeans(wts[i,,]*Conj(wts[j,,]), na.rm=TRUE)
      wavarray[j,i,]<-wavarray[i,j,]
    }
  }
  
  #modulus or real part
  if (treatment=="Mod")
  {
    wavarray<-Mod(wavarray)
  }
  if (treatment=="Re")
  {
    wavarray<-Re(wavarray)
  }
  
  return(list(timescales=timescales,wavarray=wavarray))  
}