#' Surrogate time series using Fourier surrogates
#' 
#' Creates surrogate time series using Fourier surrogates
#' 
#' @param dat A locations x time matrix of observations 
#' @param nsurrogs The number of surrogates to produce
#' @param syncpres Logical. TRUE for "synchrony preserving" surrogates (same phase randomizations used for all 
#' time series). FALSE leads to independent phase randomizations for all time series.
#'  
#' @return \code{fftsurrog} returns a list of nsurrogs surrogate datasets 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; 
#' Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references 
#' Sheppard, LW, et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' Schreiber, T and Schmitz, A (2000) Surrogate time series. Physica D 142, 346-382.
#' 
#' Prichard, D and Theiler, J (1994) Generating surrogate data for time series with several simultaneously measured variables. Physical Review Letters 73, 951-954.
#'
#' @note For internal use, no error checking
#'      
#' @importFrom stats fft rnorm

fftsurrog<-function(dat,nsurrogs,syncpres)
{
  #get ffts of all time series
  fftdat<-matrix(complex(real=NA, imaginary=NA), nrow=nrow(dat), ncol=ncol(dat))
  for(row in 1:nrow(dat))
  {
    fftdat[row,]<-stats::fft(dat[row,])
  }
  fftmod<-Mod(fftdat)
  fftarg<-Arg(fftdat)
  
  #now get random phases for each desired surrogate and
  #inverse transform to get the surrogates
  res<-list()
  for(n in 1:nsurrogs)
  {
    # get and apply random phases
    if (syncpres)
    {
      #synchrony preserving surrogates only need one set of phase pertubations, used for all time series
      h<-Arg(stats::fft(stats::rnorm(ncol(dat))))
      randomizedphases<-(matrix(rep(h, times=nrow(dat)), nrow(dat), ncol(dat), byrow=TRUE)+fftarg) %% (2*pi)
    }else
    {
      #need separate independent phase perturbations for each time series
      h<-matrix(stats::rnorm(ncol(dat)*nrow(dat)),nrow(dat),ncol(dat))
      randomizedphases<-(fftarg+t(apply(X=h,MARGIN=1,FUN=function(x){Arg(stats::fft(x))}))) %% (2*pi)
    }
    fftsurrog<-matrix(complex(modulus=fftmod, argument=randomizedphases),nrow(dat), ncol(dat))
    
    # inverse transform
    invmat<-matrix(NA, nrow(dat), ncol(dat))
    for(i in 1:nrow(dat)){
      invmat[i,]<-stats::fft(fftsurrog[i,], inverse=T)/(ncol(fftsurrog))
    }
    res[[n]]<-Re(invmat)
  }
  
  return(res)
}