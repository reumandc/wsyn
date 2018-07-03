#' Aggregate significance of coherence across a timescale band
#' 
#' Computes the aggregate significance of coherence across a timescale band, accounting for
#' non-independence of timescales. Also gets the average phase across the band.
#' 
#' @param cohobj An object of class \code{coh}, must have a non-\code{NA} \code{signif} slot
#' @param band A length-two numeric vector indicating a timescale band
#' 
#' @return \code{cohbandtest} returns an object of the same class as its first input but with a
#' \code{bandp} object added. Or if there was already a \code{bandp} object, the output has a 
#' \code{bandp} object with an additional row. The \code{bandp} object is a data frame with four
#' columns, the first two indicating the timescale band and the third an associated p-value for
#' the test of coherence over that band. The fourth column is the average phase over the band.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @export 

cohbandtest<-function(object,...)
{
  UseMethod("cohbandtest",object)
}

#' @rdname cohbandtest
#' @export
cohbandtest.default<-function(object,...)
{
  stop("Error in cohbandtest: method not defined for this class")
}

#' @rdname cohbandtest
#' @export
cohbandtest.coh<-function(cohobj,band)
{
  #error checking
  if (any(is.na(cohobj$signif)))
  {
    stop("Error in cohbandtest: signif cannot be NA")
  }
  if (!is.numeric(band))
  {
    stop("Error in cohbandtest: band must be numeric")
  }
  if (!is.vector(band))
  {
    stop("Error in cohbandtest: band must be a length-two numeric vector")
  }
  if (length(band)!=2)
  {
    stop("Error in cohbandtest: band must be a length-two numeric vector")
  }
  band<-sort(band)
  timescales<-get_timescales(cohobj)
  if (band[1]>max(timescales) || band[2]<min(timescales))
  {
    stop("Error in cohbandtest: band must include some of the timescales")
  }
  
  #add ranks if necessary
  if (any(is.na(cohobj$ranks)))
  {
    cohobj<-addranks(cohobj)
  }
  
  #get the p-value
  x<-mean(cohobj$ranks$coher[timescales>=band[1] & timescales<=band[2]]) #mean rank across timescales of interest, data
  sx<-apply(FUN=mean,X=cohobj$ranks$scoher[,timescales>=band[1] & timescales<=band[2],drop=F],MARGIN=1) #mean ranks, surrogates
  pval<-(sum(sx>=x)+1)/(length(sx)+1)
  
  #get the average phase
  x<-cohobj$coher[timescales>=band[1] & timescales<=band[2]]
  mnphs<-Arg(mean(x/Mod(x)))
  
  #form the result and return it
  if (any(is.na(cohobj$bandp)))
  {
    bandp<-data.frame(ts_low_bd=band[1],ts_hi_bd=band[2],p_val=pval,mn_phs=mnphs)    
    cohobj$bandp<-bandp
    return(cohobj)
  }else
  {
    cohobj$bandp[dim(cohobj$bandp)[1]+1,]<-c(band,pval,mnphs)
    return(cohobj)
  }
}