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
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking performed
#' 
#' @examples
#' #Not written yet but need some

addwpmfs<-function(obj,level=1:length(obj$clusters),sigmethod="quick",nrand=1000)
{
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