#' Error check wavelet transform parameters
#' 
#' Error check the parameters \code{scale.min}, \code{scale.max.input}, \code{sigma}, \code{f0}
#' 
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation that is guaranteed to be examined 
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope. Defaults to 1.
#' @param times The times data were measured at, spacing 1
#' @param callfunc Function calling this one, for better error messaging
#' 
#' @return \code{errcheck_wavparam} returns nothing but throws and error if the conditions are not met
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_wavparam<-function(scale.min,scale.max.input,sigma,f0,times,callfunc)
{
  if (!(is.numeric(scale.min) && is.numeric(sigma) && is.numeric(f0)))
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": non-numeric scale.min, sigma, or f0"))
  }
  if (!(is.numeric(scale.max.input) || is.null(scale.max.input)))
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": scale.max.input must be numeric or NULL"))
  }
  if (!(length(scale.min)==1 && length(sigma)==1 && length(f0)==1))
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": scale.min, sigma, and f0 must have length 1"))
  }
  if (!(length(scale.max.input) %in% c(0,1)))
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": scale.max.input must be NULL or of length 1"))
  }
  if (scale.min<2)
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": scale.min must be at least 2"))
  }
  if (sigma<=1)
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": sigma must be greater than 1"))
  }
  if (is.null(scale.max.input)){scale.max.input<-length(times)}
  m.max<-floor(log(scale.max.input/scale.min)/log(sigma))+1
  if (m.max<5)
  {
    stop(paste0("Error in errcheck_wavparams called by ",callfunc,": your wavelet parameters indicate you only have ",m.max," timescales, that is not very many"))
  }
}