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
#' @details The following values are valid for \code{method}: \code{"pearson"}, 
#' \code{"pearson.sig.std"}, \code{"pearson.sig.fft"}, \code{"spearman"}, \code{"spearman.sig.std"}, 
#' \code{"spearman.sig.fft"}, \code{"spearman.sig.aaft"}, \code{"kendall"}, \code{"kendall.sig.std"},
#' \code{"kendall.sig.fft"}, \code{"kendall.sig.aaft"}, \code{"ReXWT"}, \code{"ReXWT.sig.fft"}, 
#' \code{"ReXWT.sig.aaft"}, \code{"coh"}, \code{"coh.sig.fft"}, \code{"coh.sig.aaft"}, 
#' \code{"phasecoh"}, \code{"phasecoh.sig.fft"}, \code{"phasecoh.sig.aaft"}, and \code{"phase"}.
#' These identifiers correspond to the Pearson, Spearman, and Kendall correlations, the real
#' part of the cross-wavelet transform, the wavelet coherence, the wavelet phase coherence, 
#' and the average phase difference of wavelet transforms over the timescale band. 
#' 
#' Significance testing is performed using standard approaches (for correlation coefficients, 
#' although these are inappropriate for autocorrelated data), or surrogates generated using the 
#' Fourier (\code{"fft"}) or amplitude adjusted Fourier surrogates (\code{"aaft"}) methods. For 
#' \code{"coh.sig.fft"}, the fast testing algorithm of Sheppard et al. (2017) is implemented. 
#' The choice of wavelet coherence (method containing \code{"coh"}) or the real part of the 
#' cross-wavelet transform (method containing \code{"ReXWT"}) depends mainly on treatment of 
#' out-of-phase relationships. The ReXWT is more akin to a correlation coefficient in that 
#' strong in-phase relationships approach 1 and strong antiphase relationships approach -1. 
#' Wavelet coherence allows any phase relationship and ranges 0 to 1. Power normalization
#' is applied when computing coherence and real part of the cross wavelet transform. 
#' Significance tests are one-tailed. Synchrony matrices for significance-based methods when
#' \code{weighted} is \code{TRUE} contain 1 minus the p-values. 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology 
#' Letters. doi: 10.1111/ele.12782
#'  
#' @examples 
#' #Need some
#'   
#' @export
#' @importFrom stats cor cor.test
 
synmat<-function(dat,times,method,tsrange=c(0,Inf),nsurrogs=1000,
                 scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1,
                 weighted=TRUE,sigthresh=0.95)               
{
  #error checking
  errcheck_stdat(times,dat,"synmat")
  if ((!weighted) && (!grepl("sig", method)))
  { #if they use a non-significance methods and weighted is false, throw an error
    stop("Error in synmat: unweighted networks available only if method involves a significance test")  
  }

  #basic setup
  nlocs<-nrow(dat)
  ntimes<-ncol(dat)
  
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
      mat<-makeunweighted(mat,sigthresh)
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
      mat<-makeunweighted(mat,sigthresh)
    }
    return(mat)
  }
  
  #ReXWT-based methods
  if (method=="ReXWT")
  {
    #prepare wavelet transforms
    wts<-warray(dat,times,scale.min,scale.max.input,sigma,f0)
    timescales<-wts$timescales
    wts<-wts$wavarray
    
    #for normalization
    denoms<-matrix(NA,nlocs,length(timescales))
    for (i in 1:nlocs)
    {
      denoms[i,]<-sqrt(colMeans(Mod(wts[i,,]*Conj(wts[i,,])),na.rm=T))
    }
    
    #get the synchrony matrix
    mat<-matrix(NA,nlocs,nlocs)
    for (i in 2:nlocs)
    {
      for (j in 1:(i-1))
      {
        ReXWTres<-colMeans(Re(wts[i,,]*Conj(wts[j,,])), na.rm=TRUE)/(denoms[i,]*denoms[j,])
        mat[i,j]<-mean(ReXWTres[timescales >= tsrange[1] & timescales <= tsrange[2]])
        mat[j,i]<-mat[i,j]
      }
    }
    return(mat)
  }
  if (method=="ReXWT.sig.fft")
  {
    stop("Error in synmat: ReXWT.sig.fft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #HOWEVER, this should probably be done with a fast algorithm yet to be formalized. 
    #Lawrence says it should be possible to write such an algorithm. See notes in issues
    #on using the fast algorithm in the coh.sig.fft context
    #***DAN: make sure to do a one-tailed test here
  }
  if (method=="ReXWT.sig.aaft")
  {
    stop("Error in synmat: ReXWT.sig.aaft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #***DAN: make sure to do a one-tailed test here
  }
  
  #coh-based methods - we could call coh, but for speed we do it this way
  if (method=="coh")
  {
    #prepare wavelet transforms
    wts<-warray(dat,times,scale.min,scale.max.input,sigma,f0)
    timescales<-wts$timescales
    wts<-wts$wavarray
    
    #for normalization
    denoms<-matrix(NA,nlocs,length(timescales))
    for (i in 1:nlocs)
    {
      denoms[i,]<-sqrt(colMeans(Mod(wts[i,,]*Conj(wts[i,,])),na.rm=T))
    }
    
    #get the synchrony matrix
    mat<-matrix(NA,nlocs,nlocs)
    for (i in 2:nlocs)
    {
      for (j in 1:(i-1))
      {
        cohres<-Mod(colMeans(wts[i,,]*Conj(wts[j,,]),na.rm=TRUE))/(denoms[i,]*denoms[j,])
        mat[i,j]<-mean(cohres[timescales >= tsrange[1] & timescales <= tsrange[2]])
        mat[j,i]<-mat[i,j]
      }
    }
    return(mat)
  }
  if (method=="coh.sig.fft")
  {
    stop("Error in synmat: coh.sig.fft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #HOWEVER, see the notes on this subject in issues.txt
    #***DAN: make sure to do a one-tailed test here
  }
  if (method=="coh.sig.aaft")
  {
    stop("Error in synmat: coh.sig.aaft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #***DAN: make sure to do a one-tailed test here
  }
  
  #phase coherence methods - we could call coh, but for speed we do it this way
  if (method=="phasecoh")
  {
    #prepare wavelet transforms
    wts<-warray(dat,times,scale.min,scale.max.input,sigma,f0)
    timescales<-wts$timescales
    wts<-wts$wavarray
    
    #phase normalize
    wts<-wts/Mod(wts)
    
    #get the synchrony matrix
    mat<-matrix(NA,nlocs,nlocs)
    for (i in 2:nlocs)
    {
      for (j in 1:(i-1))
      {
        cohres<-Mod(colMeans(wts[i,,]*Conj(wts[j,,]),na.rm=TRUE))
        mat[i,j]<-mean(cohres[timescales >= tsrange[1] & timescales <= tsrange[2]])
        mat[j,i]<-mat[i,j]
      }
    }
    return(mat)
  }
  if (method=="phasecoh.sig.fft")
  {
    stop("Error in synmat: phasecoh.sig.fft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #***DAN: make sure to do a one-tailed test here
  }
  if (method=="phasecoh.sig.aaft")
  {
    stop("Error in synmat: phasecoh.sig.aaft option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
    #***DAN: make sure to do a one-tailed test here
  }
  
  #average phase
  if (method=="phase")
  {
    stop("Error in synmat: phase option not implemented")
    #***DAN: reumannplatz::synmat implemented this, so check there when you do this
  }
}
