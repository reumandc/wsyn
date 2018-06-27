#' Creates an array of wavelet transforms from input timeseries
#' 
#' @param dat A locations (rows) x time (columns) matrix
#' @param times A vector of timestep values (e.g. years)
#' @param scale.min The smallest scale of fluctuation that will be examined
#' @param scale.max.input The largest scale of fluctuation that will be examined. Note that if this is set too high relative to the length of the timeseries it will be truncated.
#' @param sigma The ratio of each time scale examined relative to the next timescale
#' @param f0 The ratio of the period of fluctuation to the width of the envelope
#' 
#' @return \code{warray} returns a list containing: 
#' \item{wavarray}{locations x time x timescales array of wavelet transforms}
#' \item{times}{the time steps specified (e.g., years)}
#' \item{timescales}{the timescales (1/frequency) computed for the wavelet transforms}
#' 
#' @note Important for interpreting the phase: the phases grow through time, i.e., they 
#' turn anti-clockwise. This function is internal, no error checking.
#'
#' @author Lauren Hallett, \email{hallett@@uoregon.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu};
#' Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' #***DAN has not checked this example, and it won't work until I get cleandat working anyway
#' #time<-1:30 #generate time steps
#' #dat<-matrix(rpois(20*length(time),20),nrow=20,ncol=length(time)) #generate fake count data for 20 locations
#' #dat<-CleanData(dat,normalize=F,detrend=T)$cleandat #detrend each site's time series, saving the cleaned data
#' #dat.array<-warray(dat,times=time)
#' 
#' @export

warray <- function(dat, times, scale.min=2, scale.max.input=NULL, sigma=1.05, f0 = 1)
{
  # get timescales and do first transform
  res1<-wt(dat[1,],times,scale.min,scale.max.input,sigma,f0)
  timescales<-get_timescales(res1)
  wavarray<-array(NA, dim = c(nrow(dat),  ncol(dat), length(timescales)))
  wavarray[1,,]<-get_values(res1)
  
  # populate the array with wavelet transforms
  if (nrow(dat)>=2)
  {
    for (i in 2:nrow(dat)) 
    {
      wavarray[i,,]<-get_values(wt(dat[i,], times, scale.min, scale.max.input, sigma, f0))
    }
  }
  
  return(list(wavarray=wavarray,times=times,timescales=timescales))
}