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
#' @param sigmethod Method for significance testing the wmpf, one of \code{quick}, \code{fft}, \code{aaft} (see details)
#' @param nrand The number of randomizations to be used for significance testing
#' 
#' @return \code{wpmf} returns an object of class \code{wpmf}. Slots are:
#' \item{values}{A matrix of complex numbers containing the wavelet phasor mean field, of dimensions \code{length(times)} by the number of timescales. Entries not considered reliable (longer timescales, near the edges of the time span) are set to NA.}
#' \item{times}{The times associated with the \code{wpmf}}
#' \item{timescales}{The timescales associated with the \code{wpmf}}
#' \item{signif} A list with information from the significance testing. Format depends on \code{sigmethod} (see details).
#' \item{dat}{The data matrix (locations by time) from which the \code{wpmf} was computed}
#' 
#' @note The wavelet phasor mean field was developed by Lawrence Sheppard and Daniel Reuman. R code by Thomas Anderson and Jon Walter
#' 
#' @details For \code{sigmethod} equal to \code{quick}, the empirical wpmf is compared to a distribution of 
#' magnitudes of sums of random phaors, using the same number of phasors as there are time series. The \code{signif}
#' output is a list with first element "\code{quick}" and second element a vector of \code{nrand} magnitudes of sums 
#' of random phasors. For \code{sigmethod} equal to \code{fft}, the empirical wpmf is compared to wmpfs of 
#' Fourier surrogate datasets. The \code{signif} output is a list with first element "\code{fft}", second element 
#' equal to \code{nrand}, and third element the fraction of surrogate-based wpmf magnitudes that the empirical wpmf 
#' magnitude is greater than (times by timescales matrix). For \code{sigmethod} equal to \code{aaft}, \code{aaft} 
#' surrogates are used instead. Output has similar format to the \code{fft} case. Values other than \code{quick}, 
#' \code{fft}, and \code{aaft} for \code{sigmethod} result in no significance testing.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Sheppard, L.W., et al. (2015) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @examples
#' #***DAN has not checked these yet
#' #time<-1:30
#' #obs<-matrix(rnorm(20*length(time),0,1),nrow=20,ncol=length(time))
#' #wmf<-wpmf(obs,times=time)
#' 
#' @export

wpmf<-function(dat,times,scale.min=2, scale.max.input=NULL, sigma=1.05, f0=1, sigmethod="none", nrand=1000)
{
  errcheck_stdat(times,dat,"wpmf")
  if (sigmethod %in% c("quick","fft","aaft"))
  {
    #error check rnand
    if (!is.numeric(nrand) || !is.finite(nrand) || length(nrand)!=1 || nrand<=0)
    {
      stop("Error in wpmf: inappropriate value for nrand")
    }
  }
  
  #do all the transforms
  wavarray<-warray(dat, times, scale.min, scale.max.input, sigma, f0)
  timescales<-wavarray$timescales
  wavarray<-wavarray$wavarray
  
  #make phasors, then take wpmf by averaging across space
  wavarray<-wavarray/Mod(wavarray)
  wpmfres<-apply(wavarray, c(2,3), mean, na.rm=T)
  errcheck_tts(times,timescales,wpmfres,"wpmf")
  
  #do significance testing
  signif<-NA
  if (sigmethod=="quick")
  {
    #just make nrand mags of sums of random phasors and return 
    rndphas<-matrix(complex(modulus=1,argument=2*pi*runif((dim(dat)[1])*nrand)),dim(dat)[1],nrand)
    signif<-list(sigmethod="quick",magsumrndphas=Mod(apply(FUN=mean,MARGIN=2,X=rndphas)))
  }
  if (sigmethod=="fft")
  {
    #get fft surrogates of the data that do not preserve synchrony (this is the null)
    surr<-surrog(dat,nrand,"fft",syncpres=FALSE)
    
    #get the wpmf magntude for each surrogate
    surrwpmfm<-array(NA,c(dim(wpmfres),nrand))
    for (counter in 1:nrand)
    {
      h<-wpmf(surr[[counter]],times,scale.min,scale.max.input,sigma,f0,sigmethod="none",nrand=1000)
      surrwpmfm[,,counter]<-
        Mod(get_values(h))
    }
    
    #get the fractions of surrogate wpmf magntiudes that the empirical wpmf magntiude is greater than
    gt<-matrix(NA,nrow(wpmfres),ncol(wpmfres))
    mwpmfres<-Mod(wpmfres)
    for (counter1 in 1:dim(wpmfres)[1])
    {
      for (counter2 in 1:dim(wpmfres)[2])
      {
        gt[counter1,counter2]<-sum(surrwpmfm[counter1,counter2,]<=mwpmfres[counter1,counter2])/nrand
      }
    }
    
    signif<-list(sigmethod="fft",nsurrog=nrand,gt=gt)
  }
  if (sigmethod=="aaft")
  {
    #get afft surrogates of the data that do not preserve synchrony (this is the null)
    surr<-surrog(dat,nrand,"aaft",syncpres=FALSE)
    
    #get the wpmf magnitude for each surrogate
    surrwpmfm<-array(NA,c(dim(wpmfres),nrand))
    for (counter in 1:nrand)
    {
      surrwpmfm[,,counter]<-
        Mod(get_values(wpmf(surr[[counter]],times,scale.min,scale.max.input,sigma,f0,sigmethod="none",nrand=1000)))
    }
    
    #get the fractions of surrogate wpmf magntiudes that the empirical wpmf magnitude is greater than
    gt<-matrix(NA,nrow(wpmfres),ncol(wpmfres))
    mwpmfres<-Mod(wpmfres)
    for (counter1 in 1:dim(wpmfres)[1])
    {
      for (counter2 in 1:dim(wpmfres)[2])
      {
        gt[counter1,counter2]<-sum(surrwpmfm[counter1,counter2,]<=mwpmfres[counter1,counter2])/nrand
      }
    }
    
    signif<-list(sigmethod="aaft",nsurrog=nrand,gt=gt)
  }
  
  #prepare result  
  result<-list(values=wpmfres,times=times,timescales=timescales,dat=dat,signif=signif)
  class(result)<-c("wpmf","tts","list")
  return(result)
}
