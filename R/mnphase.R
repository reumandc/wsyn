#' Mean phase of coherence
#' 
#' Gets the mean phase of a bunch of complex numbers
#'  
#' @param nums A vector of complex numbers
#' 
#' @return \code{mnphase} returns the mean phase 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal funcion, no error catching

mnphase<-function(nums)
{
  mns<-mean(nums/Mod(nums))
  res<-Arg(mns)
  res[mns==0]<-NA
  return(res)
}