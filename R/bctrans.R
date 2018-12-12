#' The one-parameter family of Box-Cox transformations
#' 
#' @param y A numeric, positive values assumed
#' @param lambda The Box-Cox parameter
#' 
#' @return \code{bctrans} gives \code{((y^lambda)-1)/lambda} for \code{lambda} not 0 or \code{ln(y)} for \code{lambda} equal to 0.
#' 
#' @details Internal function. No error checking done. It is assumed the entries of y are positive.
#'  
#' @references 
#' Box, GEP and Cox, DR (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211–252.
#' 
#' Venables, WN and Ripley, BD (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{cleandat}}, \code{browseVignettes("wsyn")}

bctrans<-function(y,lambda)
{
  if (isTRUE(all.equal(lambda,0)))
  {
    return(log(y))
  }else
  {
    return(((y^lambda)-1)/lambda)
  }
}