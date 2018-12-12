#' Adds wavelet mean field information to a \code{clust} object
#' 
#' When a \code{clust} object is created, the \code{wmfs} slot is NA. This function fills it in.
#' 
#' @param obj An object of class \code{clust}
#' 
#' @return \code{addwmfs} returns another \code{clust} object with \code{wmfs} slot now included. 
#' If  \code{obj$wmfs} was not NA, the object is returned as is.
#'
#' @details This function uses the values of \code{scale.min}, \code{scale.max.input}, 
#' \code{sigma} and \code{f0} stored in \code{obj$methodspecs}. It is possible to create 
#' a \code{clust} object with bad values for these slots. This function throws an error in that 
#' case. You can use a correlation-based method for calculating the synchrony matrix and 
#' still pass values of \code{scale.min}, \code{scale.max.input}, \code{sigma} and \code{f0} 
#' to \code{clust} (in fact, this happens by default) - they won't be used by \code{clust}, 
#' but they will be there for later use by \code{addwmfs} and \code{addwpmfs}.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{clust}}, \code{\link{addwpmfs}}, \code{browseVignettes("wsyn")}
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
#' clustobj<-clust(dat,times,coords,method,nsurrogs = 100)
#' res<-addwmfs(clustobj)
#' 
#' @export

addwmfs<-function(obj)
{
  #error checking
  if (any(class(obj)!=c("clust","list")))
  {
    stop("Error in addwmfs: obj must be a clust object")
  }
  h<-obj$methodspecs
  errcheck_wavparam(h$scale.min,h$scale.max.input,h$sigma,h$f0,obj$times,"addwmfs")
  
  #if there are NAs in wmfs, proceed, otherwise don't overwrite
  if (!any(is.na(obj$wmfs)))
  {
    return(obj)  
  }
  
  #compute the wmfs
  wmfs<-list()
  for (levcount in 1:length(obj$clusters))
  { #for each clustering level, produce a list of wmf objects which are the wmfs 
    #for the clusters that that level of clustering 
    thiswmfs<-list()
    thisclust<-obj$clusters[[levcount]]
    for (clustcount in 1:max(thisclust))
    { #do each cluster
      inds<-which(thisclust==clustcount)
      if (length(inds)==1)
      { #for clusters with one node, no wmf
        thiswmfs[[clustcount]]<-NA
      }else
      {
        thisdat<-obj$dat[thisclust==clustcount,]
        thiswmfs[[clustcount]]<-wmf(dat=thisdat,times=obj$times,
                                    scale.min=obj$methodspecs$scale.min,
                                    scale.max.input=obj$methodspecs$scale.max.input,
                                    sigma=obj$methodspecs$sigma,
                                    f0=obj$methodspecs$f0)
      }
    }
    wmfs[[levcount]]<-thiswmfs
  }
  
  #put result into the object and return
  obj$wmfs<-wmfs
  return(obj)
}