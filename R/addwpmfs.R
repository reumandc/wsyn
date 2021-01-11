#' Adds wavelet phasor mean field information to a \code{clust} object
#' 
#' When a \code{clust} object is created, the \code{wpmfs} slot is NA. This function fills it in, 
#' or adds to it.
#' 
#' @param obj An object of class \code{clust}
#' @param level The clustering level(s) to use. 1 corresponds to no clustering. The default is all 
#' levels of clustering.
#' @param sigmethod Method for significance testing the \code{wpmf}, one of \code{quick}, \code{fft}, 
#' \code{aaft} (see details of the \code{wpmf} function)
#' @param nrand The number of randomizations to be used for significance testing
#' 
#' @return \code{addwpmfs} returns another \code{clust} object with \code{wpmfs} slot now included,
#' or more filled in than it was previously. With values of \code{sigmethod} other than 
#' \code{"quick"}, this function can be slow, particularly with large \code{nrand}. So in that
#' case the user may want to set \code{level} equal only to one clustering level of interest.
#' Unlike \code{wmf}, old values in \code{obj$wpmfs} are overwritten. 
#'  
#' @details This function uses the values of \code{scale.min}, \code{scale.max.input}, 
#' \code{sigma} and \code{f0} stored in \code{obj$methodspecs}. It is possible to create 
#' a clust object with bad values for these slots. This function throws an error in that 
#' case. You can use a correlation-based method for calculating the synchrony matrix and 
#' still pass values of \code{scale.min}, \code{scale.max.input}, \code{sigma} and \code{f0} 
#' to \code{clust} (in fact, this happens by default) - they won't be used by \code{clust}, 
#' but they will be there for later use by \code{addwmfs} and \code{addwpmfs}.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{clust}}, \code{\link{addwmfs}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' sig<-matrix(.8,5,5)
#' diag(sig)<-1
#' lents<-50
#' if (requireNamespace("mvtnorm",quietly=TRUE))
#' {
#'   dat1<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
#'   dat2<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
#' }else
#' {
#'   dat1<-t(matrix(rep(rnorm(lents),times=5),lents,5))
#'   dat2<-t(matrix(rep(rnorm(lents),times=5),lents,5))
#' }
#' dat<-rbind(dat1,dat2)
#' times<-1:lents
#' dat<-cleandat(dat,times,clev=1)$cdat
#' coords<-data.frame(Y=rep(0,10),X=1:10)
#' method<-"coh.sig.fast"
#' clustobj<-clust(dat,times,coords,method,nsurrogs = 100)
#' res<-addwpmfs(clustobj)
#' 
#' @export

addwpmfs<-function(obj,level=1:length(obj$clusters),sigmethod="quick",nrand=1000)
{
  #error checking
  if (!inherits(obj,"clust"))
  {
    stop("Error in addwpmfs: obj must be a clust object")
  }
  h<-obj$methodspecs
  errcheck_wavparam(h$scale.min,h$scale.max.input,h$sigma,h$f0,obj$times,"addwpmfs")
  
  #compute the wpmfs
  wpmfs<-obj$wpmfs
  if (length(wpmfs)==1 && is.na(wpmfs))
  {
    wpmfs<-list()
  }
  for (levcount in 1:length(level))
  { #for each clustering level, produce a list of wpmf objects which are the wpmfs 
    #for the clusters that that level of clustering 
    thiswpmfs<-list()
    thislev<-level[levcount]
    thisclust<-obj$clusters[[thislev]]
    for (clustcount in 1:max(thisclust))
    { #do each cluster
      inds<-which(thisclust==clustcount)
      if (length(inds)==1)
      { #for clusters with one node, no wpmf
        thiswpmfs[[clustcount]]<-NA
      }else
      {
        thisdat<-obj$dat[thisclust==clustcount,]
        thiswpmfs[[clustcount]]<-wpmf(dat=thisdat,times=obj$times,
                                    scale.min=obj$methodspecs$scale.min,
                                    scale.max.input=obj$methodspecs$scale.max.input,
                                    sigma=obj$methodspecs$sigma,
                                    f0=obj$methodspecs$f0,
                                    sigmethod=sigmethod,
                                    nrand=nrand)
      }
    }
    wpmfs[[levcount]]<-thiswpmfs
  }
  
  #put result into the object and return
  obj$wpmfs<-wpmfs
  return(obj)
}