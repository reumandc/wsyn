#' Wavelet phasor mean field
#' 
#' Computes the wavelet phasor mean field from a matrix of spatiotemporal data. Also the
#' creator function for the \code{wpmf} class. The \code{wpmf} class inherits from the 
#' \code{tts} class, which inherits from the \code{list} class.
#' 
#' @param dat A locations (rows) x time (columns) matrix
#' @param times A vector of time step values, spacing 1
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelop
#' @param sigmethod Method for significance testing the wmpf, one of \code{quick}, \code{fft}, \code{aaft} (see details)
#' @param nrand The number of randomizations to be used for significance testing
#' 
#' @return \code{wpmf} returns an object of class \code{wpmf}. Slots are:
#' \item{values}{A matrix of complex numbers containing the wavelet phasor mean field, of dimensions \code{length(times)} by the number of timescales. Entries not considered reliable (longer timescales, near the edges of the time span) are set to NA.}
#' \item{times}{The times associated with the data and the \code{wpmf}}
#' \item{timescales}{The timescales associated with the \code{wpmf}}
#' \item{signif}{A list with information from the significance testing. Format depends on \code{sigmethod} (see details).}
#' \item{dat}{The data matrix (locations by time) from which the \code{wpmf} was computed}
#' \item{wtopt}{The inputted wavelet transform options scale.min, scale.max.input, sigma, f0 in a list}
#' 
#' @details For \code{sigmethod} equal to \code{quick}, the empirical wpmf is compared to a distribution of 
#' magnitudes of sums of random phasors, using the same number of phasors as there are time series. The \code{signif}
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
#' @seealso \code{\link{wpmf_methods}}, \code{\link{wmf}}, \code{\link{tts}}, \code{\link{plotmag}}, 
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-1:30 #generate time steps
#' #generate fake count data for 20 locations
#' dat<-matrix(rpois(20*length(times),20),nrow=20,ncol=length(times)) 
#' dat<-cleandat(dat=dat,times=times,clev=2)$cdat #detrend and demean
#' res<-wpmf(dat,times)
#' 
#' @export
#' @importFrom stats runif

wpmf<-function(dat,times,scale.min=2, scale.max.input=NULL, sigma=1.05, f0=1, sigmethod="none", nrand=1000)
{
  errcheck_stdat(times,dat,"wpmf")
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"wpmf")
  
  if (sigmethod %in% c("quick","fft","aaft"))
  {
    #error check nrand
    if (!is.numeric(nrand) || !is.finite(nrand) || length(nrand)!=1 || nrand<=0)
    {
      stop("Error in wpmf: inappropriate value for nrand")
    }
  }
  
  #for return
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
              sigma=sigma,f0=f0)
  
  #do all the transforms
  wavarray<-warray(dat, times, scale.min, scale.max.input, sigma, f0)
  timescales<-wavarray$timescales
  wavarray<-wavarray$wavarray
  
  #make phasors, then take wpmf by averaging across space
  wavarray<-normforcoh(wavarray,"phase")
  wpmfres<-apply(wavarray, c(2,3), mean, na.rm=T)
  errcheck_tts(times,timescales,wpmfres,"wpmf")
  
  #do significance testing
  signif<-NA
  if (sigmethod=="quick")
  {
    #just make nrand mags of sums of random phasors and return 
    rndphas<-matrix(complex(modulus=1,argument=2*pi*stats::runif((dim(dat)[1])*nrand)),dim(dat)[1],nrand)
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
  result<-list(values=wpmfres,times=times,timescales=timescales,signif=signif,dat=dat,wtopt=wtopt)
  class(result)<-c("wpmf","tts","list")
  return(result)
}
