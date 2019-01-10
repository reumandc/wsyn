#' Coherence
#' 
#' Wavelet coherence and wavelet phase coherence, spatial or for single time series.
#' Also the generator function for the \code{coh} class, which inherits from the \code{list}
#' class.
#' 
#' @param dat1 A locations (rows) x time (columns) matrix (for spatial coherence), or a single time series 
#' @param dat2 Same format as dat1, same locations and times
#' @param times The times at which measurements were made, spacing 1
#' @param norm The normalization of wavelet transforms to use. Controls the version of the coherence that is 
#' performed. One of "none", "phase", "powall", "powind". See details.
#' @param sigmethod The method for significance testing. One of "none", "fftsurrog1", "fftsurrog2", "fftsurrog12", 
#' "aaftsurrog1", "aaftsurrog2", "aaftsurrog12", "fast". See details.
#' @param nrand Number of surrogate randomizations to use for significance testing.
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope
#' 
#' @return \code{coh} returns an object of class \code{coh}. Slots are:
#' \item{dat1, dat2}{The input data}
#' \item{times}{The times associated with the data}
#' \item{sigmethod}{The method for significance testing, as inputted.} 
#' \item{norm}{The normalization of the wavelet transforms that will be used in computing the coherence. Different 
#' values result in different versions of the coherence. One of "none", "phase", "powall", "powind". See details.}
#' \item{wtopt}{The inputted wavelet transform options scale.min, scale.max.input, sigma, f0 in a list}
#' \item{timescales}{The timescales associated with the coherence}
#' \item{coher}{The complex magnitude of this quantity is the coherence, calculated in the usual way (which depends 
#' on \code{norm}, see details), and with scalloping of the transforms.} 
#' \item{signif}{A list with information from the significance testing. Elements are \code{coher} and \code{scoher}. 
#' See details.}
#' \item{ranks}{A list with ranking information for \code{signif}. \code{NA} until \code{plotrank} is called, see 
#' documentation for \code{plotrank}.}
#' \item{bandp}{A data frame containing results of computing significances of the coherence across timescale bands. 
#' Empty on an initial call to \code{coh}, filled in by the function \code{bandtest}. See details.}
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
#' \code{length(coher)} with rows with magnitudes equal to coherences of surrogate 
#' datasets, computed using
#' the normalization specified by \code{norm}. The type of surrogate used (Fourier surrogates 
#' or amplitude adjusted Fourier surrogates, see \code{surrog}), as well as which of the 
#' datasets surrogates are computed on (\code{dat1}, \code{dat2}, or both) is determined by 
#' \code{sigmethod}. The first part of the value of \code{sigmethod} specifies the
#' type of surrogate used, and the numbers in the second part (1, 2, or 12) specify 
#' whether surrogates are applied to \code{dat1}, \code{dat2}, or both, respectively.
#' Synchrony-preserving surrogates are used. A variety of 
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
#' when needed by other methods, see \code{plotrank} and \code{bandtest}. 
#' 
#' Regardless of what the variables represent, the normalized transform of dat1 is multiplied 
#' by the conjugate of the normalized transform of dat2. Thus, a positive phase of the coherence 
#' indicates dat1 would be leading dat2. 
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' Sheppard, L.W., et al. (2017) Rapid surrogate testing of wavelet coherences. European Physical 
#' Journal, Nonlinear and Biomedical Physics, 5, 1. DOI: 10.1051/epjnbp/2017000
#' 
#' @seealso \code{\link{cleandat}}, \code{\link{coh_methods}}, \code{\link{bandtest}}, \code{\link{plotmag}}, 
#' \code{\link{plotphase}}, \code{\link{plotrank}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-1:100
#' dat1<-matrix(rnorm(1000),10,100)
#' dat2<-matrix(rnorm(1000),10,100)
#' dat1<-cleandat(dat1,times,1)$cdat
#' dat2<-cleandat(dat2,times,1)$cdat
#' norm<-"powall"
#' sigmethod<-"fast"
#' nrand<-10
#' res<-coh(dat1,dat2,times,norm,sigmethod,nrand)
#' #for real applications, use a much bigger nrand
#' 
#' @export

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
  
  #**for return
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
              sigma=sigma,f0=f0)
  
  #**now do the different cases for how significance is computed
  
  #*no significance requested by user - just return
  if (sigmethod=="none")
  {
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,wtopt=wtopt,
                 timescales=timescales,coher=coher,signif=NA,ranks=NA,bandp=NA)
    class(result)<-c("coh","list")
    return(result)    
  }
  
  #*fast algorithm case
  if (sigmethod=="fast")
  {
    randnums<-runif(nrand*floor((ncol(dat1)-1)/2))
    if (dim(dat1)[2] %% 2 == 0)
    {
      randbits<-sample.int(2,2*nrand,replace=TRUE)-1
    }else
    {
      randbits<-sample.int(2,nrand,replace=TRUE)-1
    }
    fcres<-fastcohtest(dat1,dat2,scale.min,scale.max.input,sigma,f0,nrand,randnums,randbits,norm)
    signif<-list(coher=fcres$coher,scoher=fcres$scoher)
    
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,wtopt=wtopt,
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
               norm=norm,wtopt=wtopt,timescales=timescales,coher=coher,
               signif=signif,ranks=NA,bandp=NA)
  class(result)<-c("coh","list")
  return(result)    
}


