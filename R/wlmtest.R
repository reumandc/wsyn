#' Statistical comparison of wavelet linear models
#' 
#' Compares a wavelet linear model with a nested model. Also the generator function for 
#' the \code{wlmtest} class.
#' 
#' @param wlm A \code{wlm} object
#' @param drop Either names or indices of variables in \code{wlm$dat} that are being 
#' dropped to form the simpler, nested model. The first variable in \code{wlm$dat}, 
#' which is the response, is not allowed here.
#' @param sigmethod Method for significance testing. One of "\code{fft}", "\code{aaft}", "\code{fast}". See details.
#' 
#' @return \code{wlmtest} returns an object of class \code{wlmtest}. Slots are:
#' \item{wlm}{The input}
#' \item{drop}{The input}
#' \item{signif}{A list with information from the significance testing. Elements are 
#' \code{coher} and \code{scoher}. See details.}
#' \item{ranks}{A list with ranking information for \code{signif}. \code{NA} until 
#' \code{plotranks} is called, see documentation for \code{plotranks}.}
#' \item{bandp}{A data frame containing results of computing significances across 
#' timescale bands. Empty on an initial call to \code{coh}, filled in by the function 
#' \code{bandtest}. See details.}
#' 
#' @details 
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' Sheppard, L.W., et al. (2017) Rapid surrogate testing of wavelet coherences. European Physical 
#' Journal, Nonlinear and Biomedical Physics, 5, 1. DOI: 10.1051/epjnbp/2017000
#' Sheppard, LW et al. (2018) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas, In review.

#' @examples
#' #Not written yet but need some
#' 
#' @export



