#' Fast algorithm for significance testing coherence using Fourier surrogates
#' 
#' This is the algorithm of Sheppard et al. (2017) (see references). 
#'
#' @param dat1 A locations (rows) x time (columns) matrix (for spatial coherence), or a single time series 
#' @param dat2 Same format as \code{dat1}, same locations and times
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope
#' @param nrand Number of surrogate randomizations to use for significance testing
#' @param randnums A bunch of independent random numbers uniformly distributed on (0,1).
#' There must be \code{nrand*floor((dim(dat1)[2]-1)/2)} of these. 
#' @param randbits A bunch of random bits (0 or 1). There must be \code{nrand} of these if time
#' series are of odd length and \code{2*nrand} if even length. You may pass more than this, so,
#' in particular, you may pass \code{2*nrand} for even or odd length.
#' @param norm The normalization of wavelet transforms to use. Controls the version of the 
#' coherence that is performed. One of "none", "powall", "powind". See details in
#' the documentation of \code{coh}.
#' 
#' @return \code{fastcohtest} returns a list with these elements:
#' \item{timescales}{The timescales used}
#' \item{coher}{The magnitude of this is the fast-algorithm version of the coherence between
#' the two datasets, for comparison with \code{scoher}}
#' \item{scoher}{A matrix with \code{nrand} rows, the magnitude of each one is the 
#' fast-algorithm version of the coherence for a surrogate}
#' 
#' @author Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, minimal error checking.
#' 
#' @references 
#' Sheppard, L.W., et al. (2017) Rapid surrogate testing of wavelet coherences. European Physical 
#' Journal, Nonlinear and Biomedical Physics, 5, 1. DOI: 10.1051/epjnbp/2017000
#' 
#' @importFrom stats fft

fastcohtest<-function(dat1,dat2,scale.min,scale.max.input,sigma,f0,nrand,randnums,randbits,norm)
{
  #deal with vector datasets
  if (!is.matrix(dat1))
  {
    dat1<-matrix(dat1,1,length(dat1))
    dat2<-matrix(dat2,1,length(dat2))
  }
  
  #setup - parallels wt.R
  n<-nrow(dat1)
  tt<-ncol(dat1)
  if(is.null(scale.max.input)){scale.max<-tt}else{scale.max<-scale.max.input}
  scale.min <- f0*scale.min
  scale.max <- f0*scale.max
  m.max.wt <- floor(log(scale.max/scale.min)/log(sigma))+1 #number of timescales minus 1
  s2 <- scale.min*sigma^seq(from=0, by=1, to=m.max.wt) #widths of wavelet envelopes
  margin2 <- ceiling(sqrt(-(2*s2*s2)*log(0.5)))
  m.last <- max(which(margin2<0.5*tt))
  if (is.null(scale.max.input))
  {
    s2<-s2[1:m.last]
  }
  m.max<-length(s2) #now its number of timescales, differs by one from how m.max is used in wt.R
  
  if (length(randnums)!=nrand*floor((tt-1)/2))
  {
    stop("Error in fastcohtest: wrong length of randnums")
  }
  if (any(randnums<0 | randnums>1))
  {
    stop("Error in fastcohtest: randnums must be between 0 and 1")
  }
  if (length(randbits)<nrand)
  {
    stop("Error in fastcohtest: randbits not long enough")
  }
  if ((length(randbits)<2*nrand) && (dim(dat1)[2] %% 2 == 0))
  {
    stop("Error in fastcohtest: randbits not long enough")
  }
  if (!(all(randbits %in% c(0,1))))
  {
    stop("Error in fastcohtest: randbits can only contain 0s and 1s")
  }
  
  #Generate random phases for surrogates with correct symmetry properties 
  rrr<-2*pi*(matrix(randnums,nrow=nrand,ncol=floor((tt-1)/2))-0.5)
  if(tt%%2==0)
  { # timeseries has even length
    ts1surrang<-cbind(pi*(randbits[1:nrand]),rrr,
                      pi*(randbits[(nrand+1):(2*nrand)]),-rrr[,ncol(rrr):1])
  }
  if(tt%%2!=0)
  { # timeseries has odd length
    ts1surrang<-cbind(pi*(randbits[1:nrand]),rrr,-rrr[,ncol(rrr):1])
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
    
    res<-list(timescales=s2/f0,coher=altcoh.norm,scoher=surrcoh.norm)
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
    
    res<-list(timescales=s2/f0,coher=altcoh.norm,scoher=surrcoh.norm)  
  }
  
  #The algorithm above was developed by Lawrence Sheppard, using a Lancaster
  #convention for phase that is opposite the ordinary convention. So switch
  #phase, for phase consistency with coherence calculated in the usual way
  res$coher<-Conj(res$coher)
  res$scoher<-Conj(res$scoher)
  
  return(res)    
}