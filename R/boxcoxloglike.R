#' The log likelihood function for one-parameter Box-Cox transformation
#' 
#' @param lambda The parameter for the one-parameter family of Box-Cox transformations
#' @param dat A vector of positive values, assumed no NAs, NaNs, etc. 
#' 
#' @return \code{boxcoxloglike} returns the log likelihood associated with Box-Cox transformation with the given parameter. See Details.
#'
#' @details 
#' The log likelihood is obtained by applying the transformation \code{((dat^lambda)-1)/lambda} (for \code{lambda} 
#' not 0) or \code{ln(dat)} (for \code{lambda} equal to 0), and then performing an ordinary linear regression
#' of the result against \code{1:length(dat)} and taking the resulting log likelihood assuming a normal error 
#' structure. This is an internal function, so no error checking is done.   
#' 
#' @references 
#' Box, GEP and Cox, DR (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211–252.
#' Venables, WN and Ripley, BD (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' Sheppard, LW, et al. (2015) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#'
#' @author Daniel Reuman, \email{reuman@@ku.edu}

boxcoxloglike<-function(lambda,dat)
{
  tdat<-bctrans(dat,lambda)
  x<-1:length(tdat)
  
  #the Jacobian term, from eq. 9 of Box & Cox (1964) (see reference above)
  jacterm<-(lambda-1)*sum(log(dat))  

  loglik<-logLik(lm(tdat~x))[[1]]+jacterm
  
  return(loglik)
}