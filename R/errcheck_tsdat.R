#' Error checking whether a times vector and t.series vector make a 
#' legitimate time series for wavelet analysis
#' 
#' @param times times of measurement, spacing 1
#' @param t.series the measurements
#' @param callfunc the function from which this one was called, for error tracking
#' 
#' @return \code{errcheck_tsdat} returns nothing but throws and error if inputs not appropriate
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_tsdat<-function(times,t.series,callfunc)
{
  errcheck_times(times,callfunc)
  if (!is.numeric(t.series))
  {
    stop(paste0("Error in errcheck_tsdat called by ",callfunc,": t.series not numeric"))
  }
  if (length(times)!=length(t.series))
  {
    stop(paste0("Error in errcheck_tsdat called by ",callfunc,": times and t.series must be the same length"))
  }
  if (!all(is.finite(t.series)))
  {
    stop(paste0("Error in errcheck_times called by ",callfunc,": t.series must not contain NAs, NaNs, Infs"))
  }
  if (round(mean(t.series,na.rm=T),10) != 0)
  {
    stop(paste0("Error in errcheck_tsdat called by ",callfunc,": t.series must have zero mean"))
  }
}