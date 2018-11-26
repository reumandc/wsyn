#Simple methods of the clust class

#value setting - these just throw an error, since we do not want
#individual components of a clust object changed as that breaks the
#consistency among the components

#' @export
set_dat.clust<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a clust object")
}

#' @export
set_times.clust<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a clust object")
}

#' @export
set_coords<-function(obj,newval)
{
  UseMethod("set_coords",obj)
}

#' @export
set_coords.default<-function(obj,newval)
{
  stop("Error in set_coords: set_coords not defined for this class")
}

#' @export
set_coords.clust<-function(obj,newval)
{
  stop("Error in set_coords: coords should not be altered for a clust object")
}

#' @export
set_methodspecs<-function(obj,newval)
{
  UseMethod("set_methodspecs",obj)
}

#' @export
set_methodspecs.default<-function(obj,newval)
{
  stop("Error in set_methodspecss: set_methodspecs not defined for this class")
}

#' @export
set_methodspecs.clust<-function(obj,newval)
{
  stop("Error in set_methodspecs: methodspecs should not be altered for a clust object")
}

#' @export
set_adj<-function(obj,newval)
{
  UseMethod("set_adj",obj)
}

#' @export
set_adj.default<-function(obj,newval)
{
  stop("Error in set_adj: set_adj not defined for this class")
}

#' @export
set_adj.clust<-function(obj,newval)
{
  stop("Error in set_adj: adj should not be altered for a clust object")
}

#' @export
set_clusters<-function(obj,newval)
{
  UseMethod("set_clusters",obj)
}

#' @export
set_clusters.default<-function(obj,newval)
{
  stop("Error in set_clusters: set_clusters not defined for this class")
}

#' @export
set_clusters.clust<-function(obj,newval)
{
  stop("Error in set_clusters: clusters should not be altered for a clust object")
}

#' @export
set_modres<-function(obj,newval)
{
  UseMethod("set_modres",obj)
}

#' @export
set_modres.default<-function(obj,newval)
{
  stop("Error in set_modres: set_modres not defined for this class")
}

#' @export
set_modres.clust<-function(obj,newval)
{
  stop("Error in set_modres: modres should not be altered for a clust object")
}

#' @export
set_mns<-function(obj,newval)
{
  UseMethod("set_mns",obj)
}

#' @export
set_mns.default<-function(obj,newval)
{
  stop("Error in set_mns: set_mns not defined for this class")
}

#' @export
set_mns.clust<-function(obj,newval)
{
  stop("Error in set_mns: mns should not be altered for a clust object")
}

#' @export
set_wmfs<-function(obj,newval)
{
  UseMethod("set_wmfs",obj)
}

#' @export
set_wmfs.default<-function(obj,newval)
{
  stop("Error in set_wmfs: set_wmfs not defined for this class")
}

#' @export
set_wmfs.clust<-function(obj,newval)
{
  stop("Error in set_wmfs: wmfs should not be altered for a clust object")
}

#' @export
set_wpmfs<-function(obj,newval)
{
  UseMethod("set_wpmfs",obj)
}

#' @export
set_wpmfs.default<-function(obj,newval)
{
  stop("Error in set_wpmfs: set_wpmfs not defined for this class")
}

#' @export
set_wpmfs.clust<-function(obj,newval)
{
  stop("Error in set_wpmfs: wpmfs should not be altered for a clust object")
}

#get methods

#' @export
get_dat.clust<-function(obj)
{
  return(obj$dat)
}

#' @export
get_times.clust<-function(obj)
{
  return(obj$times)
}

#' @export
get_coords<-function(obj)
{
  UseMethod("get_coords",obj)
}

#' @export
get_coords.default<-function(obj)
{
  stop("Error in get_coords: get_coords not defined for this class")
}

#' @export
get_coords.clust<-function(obj)
{
  return(obj$coords)
}

#' @export
get_methodspec<-function(obj)
{
  UseMethod("get_methodspec",obj)
}

#' @export
get_methodspec.default<-function(obj)
{
  stop("Error in get_methodspec: get_methodspec not defined for this class")
}

#' @export
get_methodspec.clust<-function(obj)
{
  return(obj$methodspec)
}

#' @export
get_adj<-function(obj)
{
  UseMethod("get_adj",obj)
}

#' @export
get_adj.default<-function(obj)
{
  stop("Error in get_adj: get_adj not defined for this class")
}

#' @export
get_adj.clust<-function(obj)
{
  return(obj$adj)
}

#' @export
get_clusters<-function(obj)
{
  UseMethod("get_clusters",obj)
}

#' @export
get_clusters.default<-function(obj)
{
  stop("Error in get_clusters: get_clusters not defined for this class")
}

#' @export
get_clusters.clust<-function(obj)
{
  return(obj$clusters)
}

#' @export
get_modres<-function(obj)
{
  UseMethod("get_modres",obj)
}

#' @export
get_modres.default<-function(obj)
{
  stop("Error in get_modres: get_modres not defined for this class")
}

#' @export
get_modres.clust<-function(obj)
{
  return(obj$modres)
}

#' @export
get_mns<-function(obj)
{
  UseMethod("get_mns",obj)
}

#' @export
get_mns.default<-function(obj)
{
  stop("Error in get_mns: get_mns not defined for this class")
}

#' @export
get_mns.clust<-function(obj)
{
  return(obj$mns)
}

#' @export
get_wmfs<-function(obj)
{
  UseMethod("get_wmfs",obj)
}

#' @export
get_wmfs.default<-function(obj)
{
  stop("Error in get_wmfs: get_wmfs not defined for this class")
}

#' @export
get_wmfs.clust<-function(obj)
{
  return(obj$wmfs)
}

#' @export
get_wpmfs<-function(obj)
{
  UseMethod("get_wpmfs",obj)
}

#' @export
get_wpmfs.default<-function(obj)
{
  stop("Error in get_wpmfs: get_wpmfs not defined for this class")
}

#' @export
get_wpmfs.clust<-function(obj)
{
  return(obj$wpmfs)
}

#' @export
print.clust<-function(x,...)
{
  
}

#summary method when the time comes