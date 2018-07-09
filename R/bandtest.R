#' Aggregate significance across a timescale band
#' 
#' Computes the aggregate significance of coherence or of a wavelet linear model test object
#' across a timescale band, accounting for non-independence of timescales. Also gets the 
#' average phase across the band, in the case of coherence.
#' 
#' @param obj An object of class \code{coh} or \code{wlmtest}, must have a non-\code{NA} 
#' \code{signif} slot
#' @param band A length-two numeric vector indicating a timescale band
#' 
#' @return \code{bandtest} returns an object of the same class as its first input but with a
#' \code{bandp} object added. Or if there was already a \code{bandp} object, the output has a 
#' \code{bandp} object with an additional row. For a \code{coh} object, the \code{bandp} object 
#' is a data frame with four columns, the first two indicating the timescale band and the third 
#' an associated p-value for the test of coherence over that band. The fourth column is the 
#' average phase over the band. For a \code{wlmtest} object, the result is only the first three
#' of the above columns.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @export 

bandtest<-function(object,...)
{
  UseMethod("bandtest",object)
}

#' @rdname bandtest
#' @export
bandtest.default<-function(object,...)
{
  stop("Error in bandtest: method not defined for this class")
}

#' @rdname bandtest
#' @export
bandtest.coh<-function(object,band)
{
  #error checking
  if (any(is.na(object$signif)))
  {
    stop("Error in bandtest.coh: signif cannot be NA")
  }
  if (!is.numeric(band))
  {
    stop("Error in bandtest.coh: band must be numeric")
  }
  if (!is.vector(band))
  {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  if (length(band)!=2)
  {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  band<-sort(band)
  timescales<-get_timescales(object)
  if (band[1]>max(timescales) || band[2]<min(timescales))
  {
    stop("Error in bandtest.coh: band must include some of the timescales")
  }
  
  #add ranks if necessary
  if (any(is.na(object$ranks)))
  {
    object<-addranks(object)
  }
  
  #get the p-value
  x<-mean(object$ranks$coher[timescales>=band[1] & timescales<=band[2]]) #mean rank across timescales of interest, data
  sx<-apply(FUN=mean,X=object$ranks$scoher[,timescales>=band[1] & timescales<=band[2],drop=F],MARGIN=1) #mean ranks, surrogates
  pval<-(sum(sx>=x)+1)/(length(sx)+1)
  
  #get the average phase
  x<-object$coher[timescales>=band[1] & timescales<=band[2]]
  mnphs<-mnphase(x)

  #form the result and return it
  if (any(is.na(object$bandp)))
  {
    bandp<-data.frame(ts_low_bd=band[1],ts_hi_bd=band[2],p_val=pval,mn_phs=mnphs)    
    object$bandp<-bandp
    return(object)
  }else
  {
    object$bandp[dim(object$bandp)[1]+1,]<-c(band,pval,mnphs)
    return(object)
  }
}