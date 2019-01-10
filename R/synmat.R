#' Synchrony matrices
#' 
#' Calculate synchrony matrices using a variety of methods
#'
#' @param dat A locations (rows) x time (columns) matrix of measurements
#' @param times The times at which measurements were made, spacing 1
#' @param method Method for synchrony calculation. See details.
#' @param tsrange A vector containing the min and max of the focal timescale range. Defaults 
#' to all timescales that are valid given choices for scale.min, scale.max.input, f0, sigma.
#' Only used for wavelet-based methods. 
#' @param nsurrogs Number of surrogates for significance test. Defaults to 1000. Only used
#' for surrogate-based methods. 
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2. Used 
#' only for wavelet-based methods.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined. Only used 
#' for wavelet-based methods.
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be 
#' greater than 1. Only used for wavelet-based methods.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope. Only used for 
#' wavelet-based methods.
#' @param weighted If \code{TRUE}, create a weighted network. If \code{FALSE}, create a binary 
#' network using statistical significance. Binary networks are only allowed for networks based
#' on significance.
#' @param sigthresh Significance threshold needed, if \code{weighted} is false, for a network
#' link to be realized. Typically 0.95, 0.99, or 0.999, etc. Only used if \code{weighted} is
#' \code{FALSE}.
#' 
#' @return \code{synmat} returns a synchrony matrix, of type depending on the \code{method}
#' argument. See details. Diagonal entries are left as \code{NA}.
#' 
#' @details The following values are valid for \code{method}: 
#' \code{"pearson"}, \code{"pearson.sig.std"}, \code{"pearson.sig.fft"}, 
#' \code{"pearson.sig.aaft"}, 
#' \code{"spearman"}, \code{"spearman.sig.std"}, \code{"spearman.sig.fft"}, 
#' \code{"spearman.sig.aaft"}, 
#' \code{"kendall"}, \code{"kendall.sig.std"}, \code{"kendall.sig.fft"}, 
#' \code{"kendall.sig.aaft"}, 
#' \code{"ReXWT"}, \code{"ReXWT.sig.fft"}, \code{"ReXWT.sig.aaft"}, \code{"ReXWT.sig.fast"}, 
#' \code{"coh"}, \code{"coh.sig.fft"}, \code{"coh.sig.aaft"}, \code{"coh.sig.fast"},
#' \code{"phasecoh"}, \code{"phasecoh.sig.fft"}, and \code{"phasecoh.sig.aaft"}.
#' The first portions of these identifiers correspond to the Pearson, Spearman, and Kendall 
#' correlations, the real part of the cross-wavelet transform, the wavelet coherence, and the 
#' wavelet phase coherence. The second portions of these identifiers, when present, indicates
#' that significance of the measure specified in the first portion of the identifies is to
#' be used for establishing the synchrony matrix. Otherwise the value itself is used. The
#' third part of the \code{method} identifier indicates what type of significance is used.
#' 
#' Significance testing is performed using standard approaches (\code{method} flag containg
#' \code{std}; for correlation coefficients, 
#' although these are inappropriate for autocorrelated data), or surrogates generated using the 
#' Fourier (\code{method} flag containing \code{"fft"}) or amplitude adjusted Fourier 
#' surrogates (\code{"aaft"}). For 
#' \code{"coh"} and \code{"ReXWT"}, the fast testing algorithm of Sheppard et al. (2017) is also
#' implemented (\code{"fast"}). That method uses implicit Fourier surrogates. The choice of 
#' wavelet coherence (method flag containing \code{"coh"}) or the real part of 
#' the cross-wavelet 
#' transform (method flag containing \code{"ReXWT"}) depends mainly 
#' on treatment of out-of-phase 
#' relationships. The \code{"ReXWT"} is more akin to a correlation coefficient in that 
#' strong in-phase relationships approach 1 and strong antiphase relationships approach -1. 
#' Wavelet coherence allows any phase relationship and ranges from 0 to 1. Power normalization
#' is applied for \code{"coh"} and for \code{"ReXWT"}. All significance tests are one-tailed. 
#' Synchrony matrices for significance-based methods when \code{weighted} is \code{TRUE} 
#' contain 1 minus the p-values. 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Daniel Reuman, \email{reuman@@ku.edu};
#' Lei Zhao, \email{lei.zhao@@cau.edu.cn}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology 
#' Letters. doi: 10.1111/ele.12782
#'
#' @seealso \code{\link{clust}}, \code{\link{coh}}, \code{\link{surrog}}, \code{browseVignettes("wsyn")}
#'    
#' @examples 
#' sig<-matrix(.9,5,5)
#' diag(sig)<-1
#' dat1<-t(mvtnorm::rmvnorm(30,mean=rep(0,5),sigma=sig))
#' dat2<-t(mvtnorm::rmvnorm(30,mean=rep(0,5),sigma=sig))
#' dat<-rbind(dat1,dat2)
#' times<-1:30
#' dat<-cleandat(dat,times,clev=2)$cdat
#' method<-"pearson.sig.fft"
#' res<-synmat(dat,times,method,nsurrogs=100,weighted=FALSE,
#'             sigthresh=0.95)
#'   
#' @export
#' @importFrom stats cor cor.test
 
synmat<-function(dat,times,method,tsrange=c(0,Inf),nsurrogs=1000,
                 scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1,
                 weighted=TRUE,sigthresh=0.95)               
{
  #error checking
  errcheck_stdat(times,dat,"synmat")
  if (!(method %in% c("pearson","pearson.sig.std","pearson.sig.fft","pearson.sig.aaft",
                      "spearman","spearman.sig.std","spearman.sig.fft","spearman.sig.aaft",
                      "kendall","kendall.sig.std","kendall.sig.fft","kendall.sig.aaft",
                      "ReXWT","ReXWT.sig.fft","ReXWT.sig.aaft","ReXWT.sig.fast",
                      "coh","coh.sig.fft","coh.sig.aaft","coh.sig.fast",
                      "phasecoh","phasecoh.sig.fft","phasecoh.sig.aaft")))
  {
    stop("Error in synmat: bad value of method")
  }
  if ((!weighted) && (!grepl("sig", method)))
  { #if they use a non-significance methods and weighted is false, throw an error
    stop("Error in synmat: unweighted networks available only if method involves a significance test")  
  }
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"synmat")
  if (sigthresh<=0 || sigthresh>=1)
  {
    stop("Error in synmat: inappropriate value for sigthresh")
  }
  
  #basic setup
  nlocs<-nrow(dat)

  #options corresponding to one of the correlations without considering significance
  if (method %in% c("pearson","spearman","kendall"))
  {
    mat<-cor(t(dat), method=method)
    diag(mat)<-NA
    return(mat)
  }

  #options corresponding to one of the correlations, using standard significance
  if (method %in% c("pearson.sig.std","kendall.sig.std","spearman.sig.std"))
  {
    cormeth<-strsplit(method,".",fixed=TRUE)[[1]][1]
    mat<-matrix(NA,nlocs,nlocs) #compute the matrix
    for (i in 2:nlocs)
    {
      for (j in 1:(i-1))
      {
        mat[i,j]<-cor.test(dat[i,],dat[j,],method=cormeth,
                           alternative="greater")$p.value
        mat[j,i]<-mat[i,j]
      }
    }
    if (weighted)
    {
      mat<-1-mat
    }else
    {
      mat<-makeunweighted(mat,1-sigthresh)
    }
    return(mat)
  }
  
  #options corresponding to one of the correlations, using surrogate-based significance
  if (method %in% c("pearson.sig.fft","pearson.sig.aaft",
                    "kendall.sig.fft","kendall.sig.aaft",
                    "spearman.sig.fft","spearman.sig.aaft"))
  {
    #get strings specifying correlation and surrogate methods
    h<-strsplit(method,".",fixed=TRUE)[[1]]
    cormeth<-h[1]
    surrtype<-h[3]
    
    #get correlation matrices for the real data and for surrogates
    sdat<-surrog(dat,nsurrogs,surrtype,FALSE)
    cormat<-cor(t(dat), method=cormeth)
    scormat<-lapply(X=sdat,FUN=function(x){cor(t(x),method=cormeth)})
    
    #get the resulting matrix of p-values
    mat<-matrix(0,nrow(cormat),ncol(cormat))
    for (counter in 1:nsurrogs)
    {
      mat<-mat+(cormat<=scormat[[counter]])
    }
    diag(mat)<-NA
    mat<-(mat+1)/(nsurrogs+1)
    
    #convert to the synchrony matrix
    if (weighted)
    {
      mat<-1-mat
    }else
    {
      mat<-makeunweighted(mat,1-sigthresh)
    }
    return(mat)
  }
  
  #wavelet-based, non-"fast" methods
  if (method %in% c("ReXWT","ReXWT.sig.fft","ReXWT.sig.aaft",
                    "coh","coh.sig.fft","coh.sig.aaft",
                    "phasecoh","phasecoh.sig.fft","phasecoh.sig.aaft"))
  {
    #options depending on method
    h<-strsplit(method,".",fixed=TRUE)[[1]]
    if (h[1] %in% c("coh","ReXWT"))
    { #normalization methods to use on wavelet transforms
      normmeth<-"powind"
    }else
    {
      normmeth<-"phase"
    }
    if (h[1] %in% c("ReXWT"))
    { #whether to take the real part or the modulus
      treatmeth<-"Re"
    }else
    {
      treatmeth<-"Mod"
    }
    if (length(h)==1)
    {
      surrtype<-"none"
    }else
    {
      surrtype<-h[3]
    }
    
    #get pairwise coherences/ReXWTs/phase coherences
    wavarray<-wavmatwork(dat,times,scale.min,scale.max.input,sigma,f0,normmeth,treatmeth)
    timescales<-wavarray$timescales
    wavarray<-wavarray$wavarray
    
    #the case of no significance testing
    if (surrtype=="none")
    {
      mat<-apply(FUN=mean,
                 X=wavarray[,,timescales >= tsrange[1] & timescales <= tsrange[2],drop=FALSE],
                 MARGIN=c(1,2))
      return(mat)
    } #from here we are in the case of significance testing
    
    #get surrogates, and pairwise coherences/ReXWTs/phase coherences for them
    sdat<-surrog(dat,nsurrogs,surrtype,FALSE)
    swavlist<-lapply(FUN=function(x){wavmatwork(x,times,scale.min,scale.max.input,sigma,f0,normmeth,treatmeth)$wavarray},
                      X=sdat)
    swavarray<-array(NA,c(dim(wavarray),nsurrogs))
    for (counter in 1:nsurrogs)
    {
      swavarray[,,,counter]<-swavlist[[counter]]
    } #surrogates for wavarray
    
    #next comes all the ranking stuff
    rwavarray<-array(wavarray,c(dim(wavarray),nsurrogs))
    rks<-apply(FUN=sum,X=(rwavarray>swavarray),MARGIN=1:3)/nsurrogs
    srks<-(aperm(apply(X=swavarray,FUN=rank,MARGIN=1:3),c(2,3,4,1))-1)/(nsurrogs-1)
  
    #now get mean ranks for each pair of locations
    mnrks<-apply(FUN=mean,
                 X=rks[,,timescales >= tsrange[1] & timescales <= tsrange[2],drop=FALSE],
                 MARGIN=1:2)
    mnsrks<-apply(FUN=mean,
                  X=srks[,,timescales >= tsrange[1] & timescales <= tsrange[2],,drop=FALSE],
                  MARGIN=c(1,2,4))
    
    #now get p-values for each pair of locations
    mat<-matrix(0,nrow(mnrks),ncol(mnrks))
    for (counter in 1:nsurrogs)
    {
      mat<-mat+(mnrks<=mnsrks[,,counter])
    }
    diag(mat)<-NA
    mat<-(mat+1)/(nsurrogs+1)
    
    #convert to the synchrony matrix
    if (weighted)
    {
      mat<-1-mat
    }else
    {
      mat<-makeunweighted(mat,1-sigthresh)
    }
    return(mat)
  }  
  
  #fast methods
  if (method %in% c("ReXWT.sig.fast","coh.sig.fast"))
  {
    if (method=="ReXWT.sig.fast")
    {
      f<-function(x){Re(x)}
    }
    if (method=="coh.sig.fast")
    {
      f<-function(x){Mod(x)}
    }
    
    mat<-matrix(NA,nlocs,nlocs) 
    randnums<-runif(nsurrogs*floor((dim(dat)[2]-1)/2))
    if (dim(dat)[2] %% 2 == 0)
    {
      randbits<-sample.int(2,2*nsurrogs,replace=TRUE)-1
    }else
    {
      randbits<-sample.int(2,nsurrogs,replace=TRUE)-1
    }
    for (i in 2:nlocs)
    {
      for (j in 1:(i-1))
      {
        h<-fastcohtest(dat[i,],dat[j,],
                    scale.min,scale.max.input,sigma,f0,
                    nsurrogs,randnums,randbits,"powind")
        x<-f(h$coher[h$timescales >= tsrange[1] & h$timescales <= tsrange[2]])
        sx<-f(h$scoher[,h$timescales >= tsrange[1] & h$timescales <= tsrange[2],drop=F])
        
        #next comes all the ranking stuff
        rx<-matrix(x,nsurrogs,length(x),byrow = TRUE)
        rks<-apply(FUN=sum,X=(rx>sx),MARGIN=2)/nsurrogs
        srks<-(apply(X=sx,FUN=rank,MARGIN=2)-1)/(nsurrogs-1)
          
        #now get mean ranks 
        mnrks<-mean(rks)
        mnsrks<-apply(FUN=mean,MARGIN=1,X=srks)
        
        #now prepare to get the p value
        mat[i,j]<-sum(mnsrks>=mnrks)
        mat[j,i]<-mat[i,j]
      }
    }
    mat<-(mat+1)/(nsurrogs+1) #this gets the actual p-values
    if (weighted)
    {
      mat<-1-mat
    }else
    {
      mat<-makeunweighted(mat,1-sigthresh)
    }
    return(mat)
  }
}
