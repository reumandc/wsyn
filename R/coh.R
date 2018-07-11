#' Coherence
#' 
#' Wavelet coherence and wavelet phase coherence, spatial or for single time series.
#' Also the generator function for the \code{coh} class, which inherits from the \code{list}
#' class.
#' 
#' @param dat1 A locations (rows) x time (columns) matrix (for spatial coherence), or a single time series 
#' @param dat2 Same format as dat1, same locations and times
#' @param times The times at which measurements were made, spacing 1
#' @param norm The normalization of wavelet transforms to use. Controls the version of the coherence that is performed. One of "none", "phase", "powall", "powind". See details.
#' @param sigmethod The method for significance testing. One of "none", "fftsurrog1", "fftsurrog2", "fftsurrog12", "aaftsurrog1", "aaftsurrog2", "aaftsurrog12", "fast". See details.
#' @param nrand Number of surrogate randomizations to use for significance testing.
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelop
#' 
#' @return \code{coh} returns an object of class \code{coh}. Slots are:
#' \item{dat1, dat2}{The input data}
#' \item{times}{The times associated with the data}
#' \item{sigmethod}{The method for significance testing, as inputted.} 
#' \item{norm}{The normalization of the wavelet transforms that will be used in computing the coherence. Different values result in different versions of the coherence. One of "none", "phase", "powall", "powind". See details.}
#' \item{timescales}{The timescales associated with the coherence}
#' \item{coher}{The complex magnitude of this quantity is the coherence, calculated in the usual way (which depends on \code{norm}, see details), and with scalloping of the transforms.} 
#' \item{signif}{A list with information from the significance testing. Elements are \code{coher} and \code{scoher}. See details.}
#' \item{ranks}{A list with ranking information for \code{signif}. \code{NA} until \code{plotranks} is called, see documentation for \code{plotranks}.}
#' \item{bandp}{A data frame containing results of computing significances of the coherence across timescale bands. Empty on an initial call to \code{coh}, filled in by the function \code{bandtest}. See details.}
#' 
#' @details If the dimensions of \code{dat1} and \code{dat2} are \eqn{N} by \eqn{T} 
#' (\eqn{N} is 1 for 
#' vector \code{dat1} and \code{dat2}), and if the wavelet transform of the \eqn{n}th row
#' of \code{dati} is denoted \eqn{W_{i,n,\sigma}(t)}, then the coherence is the 
#' average, over all 
#' locations \eqn{n} and times \eqn{t} for which wavelet transforms are 
#' available, of the quantity 
#' \eqn{w_{1,n,\sigma}(t)w_{2,n,\sigma}(t)^{*}}, where the \eqn{*} represents 
#' complex conjugation and
#' \eqn{w_{i,n,\sigma}(t)} is a normalization of the wavelet 
#' transform. The normalization used depends 
#' on \code{norm}. If \code{norm} is "\code{none}" then raw wavelet transforms are used. 
#' If \code{norm} is "\code{phase}" then 
#' \eqn{w_{i,n,\sigma}(t)=W_{i,n,\sigma}(t)/|W_{i,n,\sigma}(t)|},
#' which gives the wavelet phase coherence, or the spatial wavelet phase coherence if \eqn{N>1}. 
#' If \code{norm} is "\code{powall}" then the normalization is that descibed in the "Wavelet 
#' mean field" section of the Methods of Sheppard et al. (2016), giving the version of the 
#' coherence that was there called simply the wavelet coherence, or the spatial wavelet 
#' coherence if \eqn{N>1}. If \code{norm} is "\code{powind}", 
#' then \eqn{w_{i,n,\sigma}(t)} is obtained
#' by dividing \eqn{W_{i,n,\sigma}(t)} by the square root of the average of 
#' \eqn{W_{i,n,\sigma}(t)W_{i,n,\sigma}(t)^{*}} over the times for 
#' which it is defined; this is done 
#' separately for each \eqn{i} and \eqn{n}.
#' 
#' The slot \code{signif} is \code{NA} if \code{sigmethod} is "\code{none}". Otherwise, and
#' if \code{sigmethod} is not "\code{fast}", then \code{signif$coher} is the same as 
#' \code{coher}, and \code{signif$scoher} is a matrix of dimensions \code{nrand} by 
#' \code{length(coher)} with rows equal to coherences of surrogate datasets, computed using
#' the normalization specified by \code{norm}. The type of surrogate used (Fourier surrogates 
#' or amplitude adjusted Fourier surrogates, see \code{surrog}), as well as which of the 
#' datasets surrogates are computed on (\code{dat1}, \code{dat2}, or both) is determined by 
#' \code{sigmethod}. Synchrony-preserving surrogates are used. A variety of 
#' statements of significance (or lack thereof) can be made
#' by comparing \code{signif$coher} with \code{signif$scoher} (see the \code{plotmag}, 
#' \code{plotrank}, and \code{bandtest} methods
#' for the \code{coh} class). If \code{sigmethod} is 
#' "\code{fast}", the fast algorithm of Sheppard et al. (2017) is used. In that case
#' \code{signif$coher} can be compared to \code{signif$scoher} to make significance 
#' statements about the coherence in exactly the same way, but \code{signif$coher} will no
#' longer precisely equal \code{coher}, and \code{coher} should not be compared 
#' directly to \code{signif$scoher}. Statements about significance of the coherence 
#' should be made using \code{signif$coher} and \code{signif$scoher}, whereas \code{coher}
#' should be used whenever the actual value of the coherence is needed. No fast algorithm
#' exists for \code{norm} equal to "\code{phase}" (the phase coherence; Sheppard et al, 2017),
#' so if \code{norm} is "\code{phase}" and \code{sigmethod} is "\code{fast}", the function
#' throws an error.
#' 
#' The slots \code{ranks} and \code{bandp} are empty on an initial call to \code{coh}. 
#' They are made to compute and hold 
#' aggregate significance results over any timescale band of choice. These are filled in
#' when needed by other methods, see \code{plotranks} and \code{bandtest}. 
#' 
#' Regardless of what the variables represent, the normalized transform of dat1 is multiplied 
#' by the conjugate of the normalized transform of dat2. Thus, positive a phase of the coherence 
#' indicates dat1 would be leading dat2. 
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' Sheppard, L.W., et al. (2017) Rapid surrogate testing of wavelet coherences. European Physical 
#' Journal, Nonlinear and Biomedical Physics, 5, 1. DOI: 10.1051/epjnbp/2017000
#' 
#' @examples
#' #Not written yet but need some
#' 
#' @export
#' @importFrom stats fft

coh<-function(dat1,dat2,times,norm,sigmethod="none",nrand=1000,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
{
  #**error checking
  errcheck_times(times,"coh")
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"coh")
  
  wasvect1<-FALSE
  if (is.matrix(dat1) && dim(dat1)[1]>1)
  {
    errcheck_stdat(1:dim(dat1)[2],dat1,"coh")
  }else
  {
    if (!is.matrix(dat1)){wasvect1<-TRUE}
    errcheck_tsdat(1:length(dat1),dat1,"coh") 
    dat1<-matrix(dat1, nrow=1, ncol=length(dat1))
  }
  wasvect2<-FALSE
  if (is.matrix(dat2) && dim(dat2)[1]>1)
  {
    errcheck_stdat(1:dim(dat2)[2],dat2,"coh")
  }else
  {
    if (!is.matrix(dat2)){wasvect2<-TRUE}
    errcheck_tsdat(1:length(dat2),dat2,"coh")
    dat2<-matrix(dat2, nrow=1, ncol=length(dat2))
  }
  if (!isTRUE(all.equal(dim(dat1),dim(dat2))))
  {
    stop("Error in coh: dimensions of dat1 and dat2 must agree") 
  }

  if (!(norm %in% c("none","phase","powall","powind")))
  {
    stop("Error in coh: bad value for norm")
  }
  if (!(sigmethod %in% c("none","fftsurrog1","fftsurrog2","fftsurrog12",
                         "aaftsurrog1","aaftsurrog2","aaftsurrog12","fast")))
  {
    stop("Error in coh: bad value for sigmethod")
  }  
  if (sigmethod=="fast" && norm=="phase")
  {
    stop("Error in coh: no fast significance algorithm for phase coherence")
  }
  
  #**get wavelet transforms
  h<-warray(dat1,times,scale.min,scale.max.input,sigma,f0)
  W1<-h$wavarray
  timescales<-h$timescales
  h<-warray(dat2,times,scale.min,scale.max.input,sigma,f0)
  W2<-h$wavarray
  
  #**normalize
  W1<-normforcoh(W1,norm)
  W2<-normforcoh(W2,norm)
  
  #**compute coherence
  coher<-apply(X=W1*Conj(W2),FUN=mean,MARGIN=3,na.rm=T)
  
  #**now do the different cases for how significance is computed
  
  #*no significance requested by user - just return
  if (sigmethod=="none")
  {
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,
                 timescales=timescales,coher=coher,signif=NA,ranks=NA,bandp=NA)
    class(result)<-c("coh","list")
    return(result)    
  }
  
  #*fast algorithm case
  if (sigmethod=="fast")
  {
    #setup
    n<-nrow(dat1)
    tt<-ncol(dat1)
    if(is.null(scale.max.input)){scale.max.input<-tt} 
    s2<-timescales*f0 #s2 is a wavelet width. We actually computed s2 in 
                      #wt.R and then got timescales by dividing by f0, so 
                      #undo that here
    m.max<-length(s2)
    
    #Generate random phases for surrogates with correct symmetry properties ##
    rrr<-2*pi*(matrix(runif(nrand*floor((tt-1)/2)),nrow=nrand,ncol=floor((tt-1)/2))-0.5)
    if(tt%%2==0)
    { # timeseries has even length
      ts1surrang<-cbind(pi*(sample.int(2,nrand,replace=T)-1),rrr,
                        pi*(sample.int(2,nrand,replace=T)-1),-rrr[,ncol(rrr):1])
    }
    if(tt%%2!=0)
    { # timeseries has odd length
      ts1surrang<-cbind(pi*(sample.int(2,nrand,replace=T)-1),rrr,-rrr[,ncol(rrr):1])
    }
    
    if ((norm %in% c("powall","none")) || (norm=="powind" && n==1))
    {
      if(n==1)
      { # One location wavelet coherence - in this case, powind is the same as powall
        fft1<-stats::fft(dat1) #fft signals 1 and 2
        fft2<-stats::fft(dat2) 
        xfft<-fft1*Conj(fft2) #get cross-spectrum and spectra
        xfft1<-fft1*Conj(fft1)
        xfft2<-fft2*Conj(fft2) 
        freqs<-seq(from=0, to=1-(1/tt), by=1/tt)
        filt.crosspec<-matrix(NA, nrow=m.max, ncol=tt) #initialize
        filt.pow1<-matrix(NA, nrow=m.max, ncol=tt)
        filt.pow2<-matrix(NA, nrow=m.max, ncol=tt)
        for(stage in 1:m.max)
        {
          s<-s2[stage]
          #find coherence by filtering cross-spectrum
          xx<-sqrt(2*pi*s)*(exp(-s^2*(2*pi*(freqs-(1-(f0/s))))^2/2) - exp(-s^2*(2*pi*freqs)^2/2)*exp(-0.5*(2*pi*f0)^2))
          m2xx<-xx*Conj(xx)/tt
          filt.crosspec[stage,]<-m2xx*xfft
          filt.pow1[stage,]<-m2xx*xfft1
          filt.pow2[stage,]<-m2xx*xfft2
        }
        altpow1<-rowMeans(filt.pow1)
        altpow2<-rowMeans(filt.pow2)
        altcoh<-rowMeans(filt.crosspec)
        if (norm %in% c("powall","powind")){altcoh.norm<-altcoh/sqrt(altpow1*altpow2)}
        if (norm=="none"){altcoh.norm<-altcoh}
        
        surrcoh<-matrix(NA, nrow=nrand, ncol=m.max)
        for(rep in 1:nrand)
        {
          ts1surrangmat<-matrix(ts1surrang[rep,], nrow=m.max, ncol=tt, byrow=T) #make surrogates
          filt.crosspec.surr<-filt.crosspec*exp(complex(imaginary=ts1surrangmat))
          surrcoh[rep,]<-rowMeans(filt.crosspec.surr)
        }
        if (norm %in% c("powall","powind")){surrcoh.norm<-surrcoh/matrix(rep(sqrt(altpow1*altpow2),each=nrow(surrcoh)),nrow(surrcoh),ncol(surrcoh))}
        if (norm=="none"){surrcoh.norm<-surrcoh}
      }
      
      ## Spatial coherence (multiple locations) - done separately from n=1 for speed reasons
      if(n>1)
      {
        fft1<-t(apply(FUN=stats::fft,MARGIN=1,X=dat1)) #fft signals 1 and 2
        fft2<-t(apply(FUN=stats::fft,MARGIN=1,X=dat2))
        xfft<-fft1*Conj(fft2) #get cross-spectra and spectra
        xfft1<-fft1*Conj(fft1)
        xfft2<-fft2*Conj(fft2)
        sxfft<-apply(xfft, 2, mean) #average cross-spectra across locations
        sxfft1<-apply(xfft1, 2, mean)
        sxfft2<-apply(xfft2, 2, mean)
        freqs<-seq(from=0, to=1-(1/tt), by=1/tt)
        filt.crosspec<-matrix(NA, nrow=m.max, ncol=tt) #initialize
        filt.pow1<-matrix(NA, nrow=m.max, ncol=tt)
        filt.pow2<-matrix(NA, nrow=m.max, ncol=tt)
        for(stage in 1:m.max)
        {
          s<-s2[stage]
          #find coherence by filtering cross-spectrum
          xx<-sqrt(2*pi*s)*(exp(-s^2*(2*pi*(freqs-(1-(f0/s))))^2/2) - exp(-s^2*(2*pi*freqs)^2/2)*exp(-0.5*(2*pi*f0)^2))
          m2xx<-xx*Conj(xx)/tt
          filt.crosspec[stage,]<-m2xx*sxfft
          filt.pow1[stage,]<-m2xx*sxfft1
          filt.pow2[stage,]<-m2xx*sxfft2
        }
        altpow1<-rowMeans(filt.pow1)
        altpow2<-rowMeans(filt.pow2)
        altcoh<-rowMeans(filt.crosspec)
        if (norm=="powall"){altcoh.norm<-altcoh/sqrt(altpow1*altpow2)}
        if (norm=="none"){altcoh.norm<-altcoh}
        
        surrcoh<-matrix(NA, nrow=nrand, ncol=m.max)
        for(rep in 1:nrand)
        {
          ts1surrangmat<-matrix(ts1surrang[rep,], nrow=m.max, ncol=tt, byrow=T) #make surrogates
          filt.crosspec.surr<-filt.crosspec*exp(complex(imaginary=ts1surrangmat))
          surrcoh[rep,]<-rowMeans(filt.crosspec.surr)
        }
        if (norm=="powall"){surrcoh.norm<-surrcoh/matrix(rep(sqrt(altpow1*altpow2),each=nrow(surrcoh)),nrow(surrcoh),ncol(surrcoh))}
        if (norm=="none"){surrcoh.norm<-surrcoh}
      }
      
      signif<-list(coher=altcoh.norm,scoher=surrcoh.norm)
    }
    
    if (norm=="powind" && n>1)
    {
      fft1<-t(apply(FUN=stats::fft,MARGIN=1,X=dat1)) #fft signals 1 and 2
      fft2<-t(apply(FUN=stats::fft,MARGIN=1,X=dat2))
      xfft1<-fft1*Conj(fft1) #get spectra
      xfft2<-fft2*Conj(fft2)
      freqs<-seq(from=0, to=1-(1/tt), by=1/tt)
      filt.crosspec<-matrix(NA, nrow=m.max, ncol=tt) #initialize
      altcoh.norm<-NA*numeric(m.max)
      for(stage in 1:m.max)
      {
        #filter the ffts
        s<-s2[stage]
        xx<-sqrt(2*pi*s)*(exp(-s^2*(2*pi*(freqs-(1-(f0/s))))^2/2) - exp(-s^2*(2*pi*freqs)^2/2)*exp(-0.5*(2*pi*f0)^2))
        xxn<-matrix(xx,n,tt,byrow=TRUE)
        filtxfft1<-xxn*Conj(xxn)*xfft1/tt
        filtxfft2<-xxn*Conj(xxn)*xfft2/tt
        
        #wavelet power for each n
        powfiltxfft1<-rowMeans(filtxfft1)
        powfiltxfft2<-rowMeans(filtxfft2)
        
        #normalize ffts
        nfft1<-fft1/sqrt(matrix(powfiltxfft1,n,tt))
        nfft2<-fft2/sqrt(matrix(powfiltxfft2,n,tt))
        
        #cross spectrum with the normalization
        nxfft<-nfft1*Conj(nfft2)
        
        sxfft<-colMeans(nxfft) #1 by tt object, average cross spectrum appropriate to this scale, normalization incorporated
        
        filt.crosspec[stage,]<-xx*Conj(xx)*sxfft/tt
      }
      #normalized coherence for this stage
      altcoh.norm<-rowMeans(filt.crosspec)
      
      surrcoh.norm<-matrix(NA, nrow=nrand, ncol=m.max)
      for(rep in 1:nrand)
      {
        ts1surrangmat<-matrix(ts1surrang[rep,], nrow=m.max, ncol=tt, byrow=T) #make surrogates
        filt.crosspec.surr<-filt.crosspec*exp(complex(imaginary=ts1surrangmat))
        surrcoh.norm[rep,]<-rowMeans(filt.crosspec.surr)
      }
      
      signif<-list(coher=altcoh.norm,scoher=surrcoh.norm)  
    }
    
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,
                 timescales=timescales,coher=coher,signif=signif,ranks=NA,bandp=NA)
    class(result)<-c("coh","list")
    return(result)    
  }
  
  #*otherwise sigmethod is one of "fftsurrog1", "fftsurrog2", 
  #"fftsurrog12", "aaftsurrog1", "aaftsurrog2", "aaftsurrog12",
  #all handled below
  
  #figure out what kind of surrogates to use
  if (sigmethod %in% c("fftsurrog1","fftsurrog2","fftsurrog12"))
  {
    surr<-"fft"
  }else
  {
    surr<-"aaft"
  }
  
  #surrogate the specified time series and take transforms and normalize
  f<-function(x,times,scale.min,scale.max.input,sigma,f0)
  {
    return(warray(x,times,scale.min,scale.max.input,sigma,f0)$wavarray)
  }
  sW1<-rep(list(W1),times=nrand)
  sW2<-rep(list(W2),times=nrand)
  if (sigmethod %in% c("fftsurrog1","fftsurrog12","aaftsurrog1","aaftsurrog12"))
  {
    sdat1<-surrog(dat1,nrand,surrtype=surr,syncpres=TRUE)
    sW1<-lapply(FUN=f,X=sdat1,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW1<-lapply(X=sW1,FUN=normforcoh,norm=norm) #normalize
  }
  if (sigmethod %in% c("fftsurrog2","fftsurrog12","aaftsurrog2","aaftsurrog12"))
  {
    sdat2<-surrog(dat2,nrand,surrtype=surr,syncpres=TRUE)
    sW2<-lapply(FUN=f,X=sdat2,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW2<-lapply(X=sW2,FUN=normforcoh,norm=norm) #normalize
  }
  
  #now compute coherences
  scoher<-matrix(complex(real=NA,imaginary=NA),nrand,length(timescales))
  for (counter in 1:nrand)
  {
    scoher[counter,]<-apply(X=sW1[[counter]]*Conj(sW2[[counter]]),FUN=mean,MARGIN=3,na.rm=T)
  }

  #assemble the significance results
  signif<-list(coher=coher,scoher=scoher)
  
  #prepare result  
  if (wasvect1){dat1<-as.vector(dat1)}
  if (wasvect2){dat2<-as.vector(dat2)}
  result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,
               norm=norm,timescales=timescales,coher=coher,signif=signif,ranks=NA,bandp=NA)
  class(result)<-c("coh","list")
  return(result)    
}


