#' Computes the wavelet phasor mean field from a matrix of spatiotemporal data. Also the
#' creator function for the \code{wpmf} class.
#' 
#' Computes the wavelet phasor mean field from a matrix of spatiotemporal data. Also the
#' creator function for the \code{wpmf} class. The \code{wpmf} class inherits from the 
#' \code{tts} class, which inherits from the \code{list} class.
#' 
#' @param dat A locations (rows) x time (columns) matrix
#' @param times A vector of time step values
#' @param scale.min The smallest scale of fluctuation that will be examined
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale
#' @param f0 The ratio of the period of fluctuation to the width of the envelop
#' 
#' @return \code{wpmf} returns an object of class \code{wpmf}. Slots are:
#' \item{values}{A matrix of complex numbers containing the wavelet phasor mean field, of dimensions \code{length(times)} by the number of timescales. Entries not considered reliable (longer timescales, near the edges of the time span) are set to NA.}
#' \item{times}{The times associated with the \code{wpmf}}
#' \item{timescales}{The timescales associated with the \code{wpmf}}
#' 
#' @note The wavelet phasor mean field was developed by Lawrence Sheppard and Daniel Reuman. R code by Thomas Anderson and Jon Walter
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Sheppard, L.W., et al. (2015) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @examples
#' time<-1:30
#' obs<-matrix(rnorm(20*length(time),0,1),nrow=20,ncol=length(time))
#' wmf<-wpmf(obs,times=time)
#' 
#' @export

wpmf<-function(dat,times,scale.min=2, scale.max.input=NULL, sigma=1.05, f0=1)
{
  freqs<-wt(dat[1,],times=1:ncol(dat), scale.min, scale.max.input, sigma, f0)$timescales
  wav.array<-warray(dat, times=1:ncol(dat), scale.min,scale.max.input, sigma, f0)$wave.array
  
  norm.array=array(NA, dim=dim(wav.array))
  for(i in 1:dim(wav.array)[1]){
    norm.array[i,,]<-wav.array[i,,]/Mod(wav.array[i,,])
  }
  
  phasor.wmf<-apply(norm.array, c(2,3), mean, na.rm=T)
  
  result<-return(list(values=phasor.wmf,times=times,timescales=freqs))
  class(result)<-c("wpmf","tts","list")
  return(result)
}
