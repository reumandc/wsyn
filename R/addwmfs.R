#' Adds wavelet mean field information to a \code{clust} object
#' 
#' When a \code{clust} object is created, the \code{wmfs} slot is NA. This function fills it in.
#' 
#' @param obj An object of class \code{clust}
#' 
#' @return \code{addwmfs} returns another \code{clust} object with \code{wmfs} slot now included. 
#' If  \code{obj$wmfs} was not NA, the object is returned as is.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @note Internal function, no error checking performed
#' 
#' @examples
#' #Not written yet but need some

addwmfs<-function(obj)
{
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