#' Error check for appropriate spatio-temporal data
#' 
#' Error checking whether a times vector and a matrix with each row a time
#' series make a ligitimate spatio-temporal data set for wavelet analysis
#' 
#' @param times the times of measurement, spacing 1
#' @param dat each row is a time series - must have at least two rows
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
  if (dim(dat)[1]==1)
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": dat must have at least two rows"))
  }
  if (length(times)!=dim(dat)[2])
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": second dimension of dat must equal length of times"))
  }
  if (!all(is.finite(dat)))
  {
    stop(paste0("Error in errcheck_stdat called by ",callfunc,": dat must not contain NAs, NaNs, Infs"))
  }
  for (counter in 1:dim(dat)[1])
  {
    errcheck_tsdat(times,dat[counter,],callfunc)
  }
}