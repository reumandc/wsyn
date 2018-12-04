#' Error check whether inputs are suitable for a tts object
#' 
#' @param times times of measurement, spacing 1
#' @param timescales timescales of analysis
#' @param values a times by timescales matrix
#' @param callfunc the function from which this one was called, for error tracking
#' 
#' @return \code{errcheck_tts} returns nothing but throws and error if inputs not appropriate
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_tts<-function(times,timescales,values,callfunc)
{
  errcheck_times(times,callfunc)
  
  #timescale should be a numeric vector with positive entries 
  if (!is.numeric(timescales))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": timescales must be numeric"))
  }
  if (length(timescales)<2)
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": timescales must be a vector"))
  }
  if (!all(is.finite(timescales)))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": timescales must not contain NAs, NaNs, Infs"))
  }
  if (any(timescales<=0))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": timescales must be positive"))
  }
  #***DAN: decreasing/increasing, what? add this once you figure it out
  
  #values a numeric or complex-valued matrix, length(times) by length(timescales)
  if (!is.numeric(values) && !is.complex(values))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": values must be numeric or complex"))
  }
  if (!is.matrix(values))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": values must be a matrix"))
  }
  if (dim(values)[1]!=length(times) || dim(values)[2]!=length(timescales))
  {
    stop(paste0("Error in errcheck_tts called by ",callfunc,": dimensions of values should agree with lengths of times and timescales"))
  }
}