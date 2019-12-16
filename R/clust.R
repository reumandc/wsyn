#' Detection and description of clusters of synchronous locations
#' 
#' Generator function for the \code{clust} S3 class, which supports tools for detecting clusters
#' (aka, modules, sub-networks, communities, etc.) of especially synchronous locations.
#' 
#' @param dat A locations (rows) x time (columns) matrix of measurements
#' @param times The times at which measurements were made, spacing 1
#' @param coords A data frame containing X,Y coordinates of locations in \code{data}, with column
#' names either \code{X} and \code{Y} or \code{lon} and \code{lat} or \code{longitude} and 
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
#' \code{FALSE}.
#' 
#' @return \code{clust} returns an object of class \code{clust}. Slots are:
#' \item{dat}{The input}
#' \item{times}{The input}
#' \item{coords}{The input}
#' \item{methodspecs}{A list with elements specifying the method used, and methodological 
#' parameters that were in the input.}
#' \item{adj}{The adjacency matrix that defines the synchrony network}
#' \item{clusters}{A list with one element for each successive split of the networks into 
#' subcomponents carried out by the clustering algorithm. Each element is a vector of length equal
#' to the number of nodes in the original network, giving cluster membership of the nodes. The 
#' first element is a vector of all 1s, corresponding to before the first clustering split was
#' performed.}
#' \item{modres}{A list of the same length as \code{clusters}, with each element containing the 
#' results of calling \code{modularity} on the network split to that level.}
#' \item{mns}{Mean time series for modules. A list of the same length as \code{clusters}.}
#' \item{wmfs}{Wavelet mean fields for modules. \code{NA} when \code{clust} is first called, but 
#' \code{addwmfs} causes this entry to be added. It is a list. See documentation for the method 
#' \code{addwmfs}.}
#' \item{wpmfs}{Wavelet phasor mean fields for modules. \code{NA} when \code{clust} is first 
#' called, but \code{addwpmfs} causes this entry to be added. It is a list. See documentation for 
#' the method \code{addwpmfs}.}
#' 
#' @details The following values are valid for \code{method}: 
#' \code{"pearson"}, \code{"pearson.sig.std"}, \code{"pearson.sig.fft"}, 
#' \code{"pearson.sig.aaft"}, 
#' \code{"spearman"}, \code{"spearman.sig.std"}, \code{"spearman.sig.fft"}, 
#' \code{"spearman.sig.aaft"}, 
#' \code{"kendall"}, \code{"kendall.sig.std"}, \code{"kendall.sig.fft"}, 
#' \code{"kendall.sig.aaft"}, 
#' \code{"ReXWT"}, \code{"ReXWT.sig.fft"}, \code{"ReXWT.sig.aaft"}, \code{"ReXWT.sig.fast"}, 
#' \code{"coh"}, \code{"coh.sig.fft"}, \code{"coh.sig.aaft"}, \code{"coh.sig.fast"},
#' \code{"phasecoh"}, \code{"phasecoh.sig.fft"}, and \code{"phasecoh.sig.aaft"}.
#' The first portions of these identifiers correspond to the Pearson, Spearman, and Kendall 
#' correlations, the real part of the cross-wavelet transform, the wavelet coherence, and the 
#' wavelet phase coherence. The second portions of these identifiers, when present, indicates
#' that significance of the measure specified in the first portion of the identifies is to
#' be used for establishing the synchrony matrix. Otherwise the value itself is used. The
#' third part of the \code{method} identifier indicates what type of significance is used.
#' 
#' Significance testing is performed using standard approaches (\code{method} flag containg
#' \code{std}; for correlation coefficients, 
#' although these are inappropriate for autocorrelated data), or surrogates generated using the 
#' Fourier (\code{method} flag containing \code{"fft"}) or amplitude adjusted Fourier 
#' surrogates (\code{"aaft"}). For 
#' \code{"coh"} and \code{"ReXWT"}, the fast testing algorithm of Sheppard et al. (2017) is also
#' implemented (\code{"fast"}). That method uses implicit Fourier surrogates. The choice of 
#' wavelet coherence (method flag containing \code{"coh"}) or the real part of 
#' the cross-wavelet 
#' transform (method flag containing \code{"ReXWT"}) depends mainly 
#' on treatment of out-of-phase 
#' relationships. The \code{"ReXWT"} is more akin to a correlation coefficient in that 
#' strong in-phase relationships approach 1 and strong antiphase relationships approach -1. 
#' Wavelet coherence allows any phase relationship and ranges from 0 to 1. Power normalization
#' is applied for \code{"coh"} and for \code{"ReXWT"}. All significance tests are one-tailed. 
#' Synchrony matrices for significance-based methods when \code{weighted} is \code{TRUE} 
#' contain 1 minus the p-values. 
#' 
#' Clustering is performed using the the eigenvector-based modularity method of 
#' Newman (2006). 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Daniel Reuman, \email{reuman@@ku.edu}; 
#' Lei Zhao, \email{lei.zhao@@cau.edu.cn}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology Letters. 
#' doi: 10.1111/ele.12782
#' 
#' Newman M.E.J. (2006). Finding community structure in networks using the eigenvectors of 
#' matrices. Phys Rev E, 74, 036104.
#' 
#' Newman M.E.J. (2006) Modularity and community structure in networks. PNAS 103, 8577-8582.
#' 
#' @seealso \code{\link{cluseigen}}, \code{\link{modularity}}, \code{\link{addwmfs}}, 
#' \code{\link{addwpmfs}},\code{\link{clust_methods}}, \code{\link{synmat}}, \code{\link{plotmap}},
#' \code{browseVignettes("wsyn")}
#'
#' @examples
#' sig<-matrix(.8,5,5)
#' diag(sig)<-1
#' lents<-50
#' dat1<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
#' dat2<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
#' dat<-rbind(dat1,dat2)
#' times<-1:lents
#' dat<-cleandat(dat,times,clev=1)$cdat
#' coords<-data.frame(Y=rep(0,10),X=1:10)
#' method<-"coh.sig.fast"
#' res<-clust(dat,times,coords,method,nsurrogs = 50)
#' #nsurrogs should be much higher for a real application
#'
#' @export

clust<-function(dat,times,coords,method,tsrange=c(0,Inf),nsurrogs=1000,
                scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1,weighted=TRUE,sigthresh=0.95)
{
  #error checking
  errcheck_stdat(times,dat,"clust")
  if (!inherits(coords,"data.frame"))
  {
    stop("Error in clust: coords must be a data frame")
  }
  if (dim(coords)[1]!=dim(dat)[1])
  {
    stop("Error in clust: coords must have one row for each row of dat")
  }
  if (!(all(c("X","Y") %in% names(coords))) && 
      !(all(c("lat","lon") %in% names(coords))) &&
      !(all(c("latitude","longitude") %in% names(coords))))
  {
    stop("Error in clust: coords must have columns X and Y, or lon and lat, or longitude and latitude")
  }
  if (!(method %in% c("pearson","pearson.sig.std","pearson.sig.fft","pearson.sig.aaft",
                      "spearman","spearman.sig.std","spearman.sig.fft","spearman.sig.aaft",
                      "kendall","kendall.sig.std","kendall.sig.fft","kendall.sig.aaft",
                      "ReXWT","ReXWT.sig.fft","ReXWT.sig.aaft","ReXWT.sig.fast",
                      "coh","coh.sig.fft","coh.sig.aaft","coh.sig.fast",
                      "phasecoh","phasecoh.sig.fft","phasecoh.sig.aaft")))
  {
    stop("Error in clust: bad value of method")
  }
  if ((!weighted) && (!grepl("sig", method)))
  { #if they use a non-significance methods and weighted is false, throw an error
    stop("Error in clust: unweighted networks available only if method involves a significance test")  
  }
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"clust")
  if (sigthresh<=0 || sigthresh>=1)
  {
    stop("Error in clust: inappropriate value for sigthresh")
  }
  
  #make methodspecs
  methodspecs<-list(method=method,tsrange=tsrange,nsurrogs=nsurrogs,
                    scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0,
                    weighted=weighted,sigthresh=sigthresh)

  #get the synchrony matrix
  adj<-synmat(dat,times,method,tsrange,nsurrogs,
              scale.min,scale.max.input,sigma,f0,
              weighted,sigthresh) 

  #do the clustering
  adjd<-adj
  diag(adjd)<-0
  clusters<-cluseigen(adjd)
    
  #get the modularities
  modres<-list()
  for (counter in 1:length(clusters))
  {
    modres[[counter]]<-modularity(adj=adjd,membership=clusters[[counter]],decomp=TRUE)
  }

  #make mean time series
  mns<-list()
  for (lcount in 1:length(clusters))
  {
    mem<-clusters[[lcount]]
    mns[[lcount]]<-matrix(NA,max(mem),length(times))
    for (ccount in 1:(max(mem)))
    {
      mns[[lcount]][ccount,]<-apply(FUN=mean,MARGIN=2,X=dat[mem==ccount,,drop=F])
    }
  }
  
  #construct the object
  result<-list(dat=dat,times=times,coords=coords,methodspecs=methodspecs,
               adj=adj,clusters=clusters,modres=modres,mns=mns,
               wmfs=NA,wpmfs=NA)
  class(result)<-c("clust","list")
  return(result)    
}