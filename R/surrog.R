#' Creates surrogate time series, either Fourier surrogates or amplitude adjusted 
#' Fourier surrogates
#' 
#' For significance testing wavelet coherence and other purposes
#' 
#' @param dat A locations x time matrix of observations (for multiple-time series input), or a single vector
#' @param nsurrogs The number of surrogates to produce
#' @param surrtype Either "fft" (for Fourier surrogates) or "aaft" for amplitude adjusted Foutier surrogates. Fourier surrogates are appropriate for time series with normal marginals; otherwise consider aaft surrogates.
#' @param syncpres Logical. TRUE for "synchrony preserving" surrogates (same phase randomizations used for all time series). FALSE leads to independent phase randomizations for all time series.
#'  
#' @return \code{surrog} returns a list of nsurrogs surrogate datasets 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; 
#' Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references 
#' Sheppard, LW, et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' Schreiber, T and Schmitz, A (2000) Surrogate time series. Physica D 142, 346-382.
#' Prichard, D and Theiler, J (1994) Generating surrogate data for time series with several simultaneously measured variables. Physical Review Letters 73, 951-954.
#'     
#' @examples
#' #Not yet written, but need some
#' 
#' @export 

surrog<-function(dat,nsurrogs,surrtype,syncpres)
{
  #error check
  wasvect<-FALSE
  if (is.matrix(dat))
  {
    errcheck_stdat(1:dim(dat)[2],dat,"surrog")
  }else
  {
    errcheck_tsdat(1:length(dat),dat,"surrog")
    dat<-matrix(dat, nrow=1, ncol=length(dat))
    wasvect<-TRUE
  }
  if (!(surrtype %in% c("fft","aaft")))
  {
    stop("Error in surrog: bad value for surrtype")
  }

  #aaft surrogates  
  if (surrtype=="aaft")
  {
    stop("Error in surrog: aaft surrogates not implemented yet")
    #Q - break ties and map separately for each time series? or by combining all values into one pot?
    #A - separately
    
    #outline:
    # if none of the timeseries have any within-timeseries ties
    #   map each time series (separately) onto the quantiles of a normal dist
    #   apply fft surrogates as below
    #   remap results back 
    # if there are ties
    #   break the ties randomly (use the rank function) while mapping onto quantiles of a normal (this has to be done once for each surrogate, since it is random)
    #   randomize the phases of each of these
    #   inverse transform
    #   remap, undoing the first step (so you will have to retain each mapping)
    # Probably these two alternatives have to be done separately, because for the second you have to fft each random mapping of the data to normal quantiles
    # Note: the rank function will come in handy because it can randomly break ties or not, according to an option
  } # Note: the case of ties is much less efficient because two additional steps have to be done separately for all surrogs
  
  #fft surrogates
  if (surrtype=="fft")
  {
    #get ffts of all time series
    fftdat<-matrix(complex(real=NA, imaginary=NA), nrow=nrow(dat), ncol=ncol(dat))
    for(row in 1:nrow(dat))
    {
      fftdat[row,]<-fft(dat[row,])
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
        h<-Arg(fft(rnorm(ncol(dat))))
        randomizedphases<-(matrix(rep(h, times=nrow(dat)), nrow(dat), ncol(dat), byrow=TRUE)+fftarg) %% (2*pi)
      }else
      {
        #need separate independent phase perturbations for each time series
        h<-matrix(rnorm(ncol(dat)*nrow(dat)),nrow(dat),ncol(dat))
        randomizedphases<-(fftarg+t(apply(X=h,MARGIN=1,FUN=function(x){Arg(fft(x))}))) %% (2*pi)
      }
      fftsurrog<-matrix(complex(modulus=fftmod, argument=randomizedphases),nrow(dat), ncol(dat))
      
      # inverse transform
      invmat<-matrix(NA, nrow(dat), ncol(dat))
      for(i in 1:nrow(dat)){
        invmat[i,]<-fft(fftsurrog[i,], inverse=T)/(ncol(fftsurrog))
      }
      if (wasvect)
      {
        res[[n]]<-as.vector(Re(invmat))
      }else
      {
        res[[n]]<-Re(invmat)
      }
    }
    return(res)
  }
}