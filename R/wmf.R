#' Computes the wavelet mean field from a matrix of spatiotemporal data. Also the
#' creator function for the \code{wmf} class.
#' 
#' Computes the wavelet mean field from a matrix of spatiotemporal data. Also the
#' creator function for the \code{wmf} class. The \code{wmf} class inherits from the 
#' \code{tts} class, which inherits from the \code{list} class.
#' 
#' @param dat A locations (rows) x time (columns) matrix
#' @param times A vector of time step values (e.g., years), spacing 1
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation that will be examined. Note that if this is set too high relative to the length of the timeseries it will be truncated.
#' @param sigma The ratio of each time scale examined relative to the next timescale. Greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope
#' 
#' @return \code{wmf} returns an object of class \code{wmf}. Slots are:
#' \item{values}{A matrix of complex numbers containing the wavelet mean field, of dimensions \code{length(times)} by the number of timescales. Entries not considered reliable (longer timescales, near the edges of the time span) are set to NA.}
#' \item{times}{The time steps specified (e.g., years)}
#' \item{timescales}{The timescales (1/frequency) computed for the wavelet transforms}
#' \item{dat}{The data matrix (locations by time) from which the wmf was computed}
#' 
#' @note The wavelet mean field was developed by Lawrence Sheppard and Daniel Reuman. R code by Jonathan Walter.
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Sheppard, L.w., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @examples
#' #***DAN has not reviewed this example yet, and it wont work until I get cleandat working
#' #time<-1:30 #generate time steps
#' #dat<-matrix(rpois(20*length(time),20),nrow=20,ncol=length(time)) #generate fake count data for 20 locations
#' #dat<-cleandat(dat,normalize=F,detrend=T)$cleandat #detrend each site's time series, saving the cleaned data
#' #wmf<-wmf(dat,times=time)
#' 
#' @export

wmf<-function(dat, times, scale.min=2, scale.max.input=NULL, sigma=1.05, f0 = 1){
  
  #check suitability of data
  errcheck_stdat(times,dat,"wmf")
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"wmf")
  
  #do all the transforms
  wavarray<-warray(dat, times, scale.min, scale.max.input, sigma, f0)
  timescales<-wavarray$timescales
  wavarray<-wavarray$wavarray
  
  #Get square modulus, then average over time and location and take square root to 
  #get denominator for normalization
  #Then normalize each timescale by the value of the normalization denomenator for 
  #that timescale
  wavarray<-normforcoh(wavarray,"powall")

  #get the wmf by averaging across space
  wmf<-apply(wavarray, c(2,3), mean, na.rm=T)
  
  #prepare the result
  errcheck_tts(times,timescales,wmf,"wmf")
  result<-list(values=wmf,times=times,timescales=timescales,dat=dat)
  class(result)<-c("wmf","tts","list")
  return(result)
}