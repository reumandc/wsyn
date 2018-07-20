#' Detection and description of clusters of synchronous locations
#' 
#' Generator function for the \code{clust} S3 class, which supports tools for detecting clusters
#' (aka, modules, sub-networks, communities, etc.) of especially synchronous locations.
#' 
#' @param dat A locations (rows) x time (columns) matrix of measurements
#' @param times The times at which measurements were made, spacing 1
#' @param coords A data frame containing X,Y coordinates of locations in \code{data}, with column
#' names either \code{X} and \code{Y} or \code{long} and \code{lat} or \code{longitude} and 
#' \code{latitude}. The data frame may contain other columns with additional metainformation 
#' about the sites.
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
#' @return \code{clust} returns an object of class \code{clust}. Slots are:
#' \item{dat}{The input}
#' \item{times}{The input}
#' \item{coords}{The input}
#' \item{methodspecs}{A list with elements specifying the method used. The first entry is the input
#' \code{method}. Subsequent entries are those inputs needed to fully specify all parameters of the 
#' method. The inputs needed and included here depend on the value of \code{method}.}
#' \item{adj}{The adjacency matrix that defines the synchrony network}
#' \item{clusters}{A list with one element for each successive split of the networks into 
#' subcomponents carried out by the clustering algorithm. Each element is a vector of length equal
#' to the number of nodes in the original network, giving cluster membership of the nodes. The 
#' first element is a vector of all 1s, corresponding to before the first clustering split was
#' performed.}
#' \item{modres}{A list of the same length as \code{clusters}, with each element containing the 
#' results of calling \code{modularity} on the network split to that level.}
#' \item{mns}{Mean time series for modules. \code{NA} when \code{clust} is first called, but 
#' \code{addmeans} adds this entry, which is a list. See documentation for the method 
#' \code{addmeans}.}
#' \item{wmfs}{Wavelet mean fields for modules. \code{NA} when \code{clust} is first called, but 
#' \code{addwmfs} adds this entry, which is a list. See documentation for the method 
#' \code{addwmfs}.}
#' \item{wpmfs}{Wavelet phasor mean fields for modules. \code{NA} when \code{clust} is first 
#' called, but \code{addwpmfs} adds this entry, which is a list. See documentation for the method
#' \code{addwpmfs}.}
#' 
#' @details 
#' ***DAN: Incorporate the below info when you write this
#' For syn.method involving significance testing, 1-p can be used in weighted networks. Currently the eigenvector-based
#' modularity method of Newman (2006) is the only implemented algorithm for detecting modules. 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology Letters. 
#' doi: 10.1111/ele.12782
#'  
#' @examples
#' #Not written yet but need some
#' 
#' @export

clust<-function(dat,times,coords,method,tsrange=c(0,Inf),nsurrogs=1000,
                scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1,weighted=TRUE,sigthresh=0.95)
{
  
}