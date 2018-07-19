#' Synchrony matrices
#' 
#' Calculate synchrony matrices using a variety of methods
#'
#' @param dat A locations (rows) x time (columns) matrix of measurements
#' @param times The times at which measurements were made, spacing 1
#' @param method Method for synchrony calculation. See details.
#' @param tsrange A vector containing the min and max of the focal timescale range. Defaults 
#' to all timescales that are valid given choices for scale.min, scale.max.input, f0, sigma.
#' Only used for wavelet-based methods. 
#' @param nsurrogs Number of surrogates for significance test. Defaults to 1000. Only used
#' for surrogate-based methods. 
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2. Used 
#' only for wavelet-based methods.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined. Only used 
#' for wavelet-based methods.
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be 
#' greater than 1. Only used for wavelet-based methods.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope. Only used for 
#' wavelet-based methods.
#' @param weighted If \code{TRUE}, create a weighted network. If \code{FALSE}, create a binary 
#' network using statistical significance. Binary networks are only allowed for networks based
#' on significance.
#' @param sigthresh Significance threshold needed, if \code{weighted} is false, for a network
#' link to be realized. Typically 0.95, 0.99, or 0.999, etc. Only used if \code{weighted} is
#' \code{FALSE}
#' 
#' @return \code{synmat} returns a synchrony matrix, of type depending on the \code{method}
#' argument. See details.
#' 
#' @details The following values are valid for \code{method}: \code{"pearson"}, 
#' \code{"pearson.sig.std"}, \code{"pearson.sig.fft"}, \code{"spearman"}, \code{"spearman.sig.std"}, 
#' \code{"spearman.sig.fft"}, \code{"spearman.sig.aaft"}, \code{"kendall"}, \code{"kendall.sig.std"},
#' \code{"kendall.sig.fft"}, \code{"kendall.sig.aaft"}, \code{"ReXWT"}, \code{"ReXWT.sig.fft"}, 
#' \code{"ReXWT.sig.aaft"}, \code{"coh"}, \code{"coh.sig.fft"}, \code{"coh.sig.aaft"}, 
#' \code{"phasecoh"}, \code{"phasecoh.sig.fft"}, \code{"phasecoh.sig.aaft"}, and \code{"phase"}.
#' These identifiers correspond to the Pearson, Spearman, and Kendall correlations, the real
#' part of the cross-wavelet transform, the wavelet coherence, the wavelet phase coherence, 
#' and the average phase difference of wavelet transforms over the timescale band. Significance 
#' testing is performed using standard approaches (for correlation coefficients, although these
#' are inappropriate for autocorrelated data), or surrogates generated using the Fourier 
#' (\code{"fft"}) or amplitude adjusted Fourier surrogates (\code{"aaft"}) methods. For 
#' \code{"coh.sig.fft"}, the fast testing algorithm of Sheppard et al. (2017) is implemented. 
#' The choice of wavelet coherence (method containing \code{"coh"}) or the real part of the 
#' cross-wavelet transform (method containing \code{"ReXWT"}) depends mainly on treatment of 
#' out-of-phase relationships. The ReXWT is more akin to a correlation coefficient in that 
#' strong in-phase relationships approach 1 and strong antiphase relationships approach -1. 
#' Wavelet coherence allows any phase relationship and ranges 0 to 1.
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology 
#' Letters. doi: 10.1111/ele.12782
#'  
#' @examples 
#' #Need some
#'   
#' @export

synmat<-function(dat,times,method,tsrange=c(0,Inf),nsurrogs=1000,
                 scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1,weighted=TRUE,sigthresh=0.95)               )
{
  
  
  return(mat) 
}
