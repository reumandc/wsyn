#' Error checking whether a times vector and a matrix with each row a time
#' series make a ligitimate spatio-temporal data set for wavelet analysis
#' 
#' @param times the times of measurement
#' @param dat each row is a time series
#' @param callfunc the function calling this one, for error tracking
#' 
#' @return \code{errcheck_stdat} returns nothing but throws and error if inputs not appropriate
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_stdat<-function(times,dat,callfunc)
{
  errcheck_times(times,callfunc)
  if (!is.numeric(dat))
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": dat must be numeric"))
  }
  if (!is.matrix(dat))
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": dat must be a matrix"))
  }
  if (length(times)!=dim(dat)[2])
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": dim(dat)[2] must equal length(times)"))
  }
  for (counter in 1:dim(dat)[1])
  {
    errcheck_tsdat(times,dat[counter,],callfunc)
  }
}