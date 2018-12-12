#' Shifts a vector according to the argument mints
#' 
#' @param ts A vector of numeric values representing a time series
#' @param mints The time series is shifted to have this minimum value. Default NA means use the smallest difference 
#' between consecutive, distinct sorted values of the time series. NaN means perform no shift.
#' 
#' @return \code{setmints} returns the shifted vector.
#' 
#' Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @note This is an internal function, and no error checking is done.
 
setmints<-function(ts,mints)
{
  if (!is.nan(mints))
  {
    if (is.na(mints))
    {
      diffs<-diff(sort(ts))
      mints<-min(diffs[diffs!=0]) 
    }
    ts<-ts-min(ts)+mints
  }
  return(ts)
}