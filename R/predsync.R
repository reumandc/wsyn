#' Predicted synchrony of a wavelet linear model
#' 
#' Predicted synchrony of a \code{wlm} object. This is described in the
#' first paragraph of Appendix S15 of Sheppard et al (2018).
#' 
#' @param wlmobj A \code{wlm} object
#' 
#' @return \code{predsync} returns a \code{tts} object. Plotting the magnitude
#' (see \code{plotmag}) displays a picture of predicted synchrony versus time and
#' timescale that is comparable with the wavelet mean field (see \code{wmf}) of
#' the response variable of the model. Calling the \code{power} function on that 
#' \code{tts} object should give the same results as one of the columns of output
#' of \code{syncexpl}. Only \code{norm="powall"} implemented so far.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, LW et al. (2018) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas, In review.
#' 
#' @examples
#' #Not written yet but need some
#' 
#' @export

predsync<-function(object,...)
{
  UseMethod("predsync",object)
}

#' @export
predsync.default<-function(object,...)
{
  stop("Error in predsync: method not defined for this class")
}

#' @rdname predsync
#' @export
predsync.wlm<-function(wlmobj)
{
  #get the necessary slots
  modval<-get_modval(wlmobj)
  coher<-get_coher(wlmobj)
  times<-get_times(wlmobj)
  timescales<-get_timescales(wlmobj)
  norm<-get_norm(wlmobj)
  
  #only powall implemented
  if (norm!="powall")
  {
    stop("Error in predsync: this value of norm not implemented yet")
  }
  
  #get the model-predicted synchrony
  modval<-normforcoh(modval,norm)
  res<-apply(FUN=mean,X=modval,MARGIN=c(2,3)) #model mean field  
  for (tscounter in 1:length(timescales))
  { #multiply by model-response coherence
    res[,tscounter]<-res[,tscounter]*coher[tscounter]
  }
  
  #prepare result and return
  res<-tts(times,timescales,res)
  return(res)
}
