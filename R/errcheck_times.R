#' Error check whether a vector can represent times at which data suitable 
#' for wavelet transforms were measured
#' 
#' @param times Tests whether this is a numeric vector with evenly spaced increasing values
#' @param callfunc Function calling this one, for better error messaging
#' 
#' @return \code{errcheck_times} returns nothing but throws and error if the conditions are not met
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_times<-function(times,callfunc)
{
  if (!is.numeric(times))
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": times must be numeric"))
  }
  if (length(times)<2)
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": times must be a vector"))
  }
  if (!all(is.finite(times)))
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": times must not contain NAs, NaNs, Infs"))
  }
  d<-diff(times)
  if (!isTRUE(all.equal(rep(d[1],length(d)-1),d[2:length(d)])))
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": times must be evenly spaced"))  
  }
  if (d[1]<=0)
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": times must be increasing"))
  }
}
