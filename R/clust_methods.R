#' Basic methods for the \code{clust} class
#' 
#' Set, get, summary, and print methods for the \code{clust} class.
#' 
#' @param object,x,obj An object of class \code{clust}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.clust} produces a summary of a \code{clust} object.
#' A \code{print.clust} method is also available. For \code{clust} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{clust} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{clust} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{clust}}
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
#' h<-clust(dat,times,coords,method,nsurrogs = 50)
#' #nsurrogs should be much higher for a real application
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name clust_methods
NULL
#> NULL

#' @rdname clust_methods
#' @export
summary.clust<-function(object,...)
{
  x<-object
  
  ms<-x$methodspecs
  h<-ms$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  #whether the wmfs slot is empty or filled
  if (class(x$wmfs)=="list")
  {
    h2<-"filled"
  }else
  {
    h2<-"empty"
  }
  
  #same for wpmfs slot
  if (class(x$wpmfs)=="list")
  {
    h3<-"filled"
  }else
  {
    h3<-"empty"
  }
  
  res<-list(class="clust",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            sampling_locs=dim(x$dat)[1],
            method=ms$method,
            tsrange1=ms$tsrange[1],
            tsrange2=ms$tsrange[2],
            nsurrogs=ms$nsurrogs,
            weighted=ms$weighted,
            sigthresh=ms$sigthresh,
            scale.min=ms$scale.min,
            scale.max.input=h,
            sigma=ms$sigma,
            f0=ms$f0,
            num_split_steps=length(x$clusters)-1,
            num_final_modules=max(x$clusters[[length(x$clusters)]]),
            final_modularity=x$modres[[length(x$modres)]]$totQ,
            wmf_slot_is=h2,
            wpmf_slot_is=h3)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname clust_methods
#' @export
print.clust<-function(x,...)
{
  cat("clust object:\n")
  
  #info on times
  cat("times, a length",length(x$times),"numeric vector:\n")
  if (length(x$times)<12)
  {
    cat(paste(x$times),"\n")  
  }else
  {
    cat(paste(x$times[1:5]),"...",paste(x$times[(length(x$times)-4):(length(x$times))]),"\n")
  }
  
  #number of sampling locations
  cat("Number of sampling locations:",dim(x$dat)[1],"\n")
  
  #summary of methodspecs
  cat("methodspecs:\n")
  w<-x$methodspecs
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("method=",w$method,"; tsrange=",w$tsrange[1]," to ",w$tsrange[2],"; nsurrogs=",w$nsurrogs,"; weighted=",w$weighted,"; sigthresh=",w$sigthresh,
      ";\nscale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,
      "\n",sep="")
  
  #number of non-zeros in adj and range of values
  cat("adj has",sum(x$adj!=0,na.rm=TRUE),"of",prod(dim(x$adj))-dim(x$adj)[1],
      "off-diagonal entries differing from 0; values range from",min(x$adj,na.rm=T),"to",max(x$adj,na.rm=T),"\n")
  
  #number of splits done, number of clusters in final decomp
  cat("Number of splitting steps done:",length(x$clusters)-1,"\n")
  cat("Number of modules in final decomposition:",length(unique(x$clusters[[length(x$clusters)]])),"\n")
  
  #modularity values for each level
  res<-c()
  for (counter in 1:length(x$modres))
  {
    res<-c(res,x$modres[[counter]]$totQ)
  }
  cat("Modularity values for each step:",paste(res),"\n")
  
  #whether the wmfs slot is empty or filled
  if (class(x$wmfs)=="list")
  {
    cat("The wmfs slot is: filled\n")
  }else
  {
    cat("The wmfs slot is: empty\n")  
  }
  
  #same for wpmfs slot
  if (class(x$wpmfs)=="list")
  {
    cat("The wpmfs slot is: filled\n")
  }else
  {
    cat("The wpmfs slot is: empty\n")  
  }
}

#' @rdname clust_methods
#' @export
set_times.clust<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_adj<-function(obj,newval)
{
  UseMethod("set_adj",obj)
}

#' @rdname setget_methods
#' @export
set_adj.default<-function(obj,newval)
{
  stop("Error in set_adj: set_adj not defined for this class")
}

#' @rdname clust_methods
#' @export
set_adj.clust<-function(obj,newval)
{
  stop("Error in set_adj: adj should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_clusters<-function(obj,newval)
{
  UseMethod("set_clusters",obj)
}

#' @rdname setget_methods
#' @export
set_clusters.default<-function(obj,newval)
{
  stop("Error in set_clusters: set_clusters not defined for this class")
}

#' @rdname clust_methods
#' @export
set_clusters.clust<-function(obj,newval)
{
  stop("Error in set_clusters: clusters should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_modres<-function(obj,newval)
{
  UseMethod("set_modres",obj)
}

#' @rdname setget_methods
#' @export
set_modres.default<-function(obj,newval)
{
  stop("Error in set_modres: set_modres not defined for this class")
}

#' @rdname clust_methods
#' @export
set_modres.clust<-function(obj,newval)
{
  stop("Error in set_modres: modres should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_mns<-function(obj,newval)
{
  UseMethod("set_mns",obj)
}

#' @rdname setget_methods
#' @export
set_mns.default<-function(obj,newval)
{
  stop("Error in set_mns: set_mns not defined for this class")
}

#' @rdname clust_methods
#' @export
set_mns.clust<-function(obj,newval)
{
  stop("Error in set_mns: mns should not be altered for a clust object")
}

#' @rdname clust_methods
#' @export
set_dat.clust<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_coords<-function(obj,newval)
{
  UseMethod("set_coords",obj)
}

#' @rdname setget_methods
#' @export
set_coords.default<-function(obj,newval)
{
  stop("Error in set_coords: set_coords not defined for this class")
}

#' @rdname clust_methods
#' @export
set_coords.clust<-function(obj,newval)
{
  stop("Error in set_coords: coords should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_methodspecs<-function(obj,newval)
{
  UseMethod("set_methodspecs",obj)
}

#' @rdname setget_methods
#' @export
set_methodspecs.default<-function(obj,newval)
{
  stop("Error in set_methodspecss: set_methodspecs not defined for this class")
}

#' @rdname clust_methods
#' @export
set_methodspecs.clust<-function(obj,newval)
{
  stop("Error in set_methodspecs: methodspecs should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_wmfs<-function(obj,newval)
{
  UseMethod("set_wmfs",obj)
}

#' @rdname setget_methods
#' @export
set_wmfs.default<-function(obj,newval)
{
  stop("Error in set_wmfs: set_wmfs not defined for this class")
}

#' @rdname clust_methods
#' @export
set_wmfs.clust<-function(obj,newval)
{
  stop("Error in set_wmfs: wmfs should not be altered for a clust object")
}

#' @rdname setget_methods
#' @export
set_wpmfs<-function(obj,newval)
{
  UseMethod("set_wpmfs",obj)
}

#' @rdname setget_methods
#' @export
set_wpmfs.default<-function(obj,newval)
{
  stop("Error in set_wpmfs: set_wpmfs not defined for this class")
}

#' @rdname clust_methods
#' @export
set_wpmfs.clust<-function(obj,newval)
{
  stop("Error in set_wpmfs: wpmfs should not be altered for a clust object")
}

#' @rdname clust_methods
#' @export
get_times.clust<-function(obj)
{
  return(obj$times)
}

#' @rdname setget_methods
#' @export
get_adj<-function(obj)
{
  UseMethod("get_adj",obj)
}

#' @rdname setget_methods
#' @export
get_adj.default<-function(obj)
{
  stop("Error in get_adj: get_adj not defined for this class")
}

#' @rdname clust_methods
#' @export
get_adj.clust<-function(obj)
{
  return(obj$adj)
}

#' @rdname setget_methods
#' @export
get_clusters<-function(obj)
{
  UseMethod("get_clusters",obj)
}

#' @rdname setget_methods
#' @export
get_clusters.default<-function(obj)
{
  stop("Error in get_clusters: get_clusters not defined for this class")
}

#' @rdname clust_methods
#' @export
get_clusters.clust<-function(obj)
{
  return(obj$clusters)
}

#' @rdname setget_methods
#' @export
get_modres<-function(obj)
{
  UseMethod("get_modres",obj)
}

#' @rdname setget_methods
#' @export
get_modres.default<-function(obj)
{
  stop("Error in get_modres: get_modres not defined for this class")
}

#' @rdname clust_methods
#' @export
get_modres.clust<-function(obj)
{
  return(obj$modres)
}

#' @rdname setget_methods
#' @export
get_mns<-function(obj)
{
  UseMethod("get_mns",obj)
}

#' @rdname setget_methods
#' @export
get_mns.default<-function(obj)
{
  stop("Error in get_mns: get_mns not defined for this class")
}

#' @rdname clust_methods
#' @export
get_mns.clust<-function(obj)
{
  return(obj$mns)
}

#' @rdname clust_methods
#' @export
get_dat.clust<-function(obj)
{
  return(obj$dat)
}

#' @rdname setget_methods
#' @export
get_coords<-function(obj)
{
  UseMethod("get_coords",obj)
}

#' @rdname setget_methods
#' @export
get_coords.default<-function(obj)
{
  stop("Error in get_coords: get_coords not defined for this class")
}

#' @rdname clust_methods
#' @export
get_coords.clust<-function(obj)
{
  return(obj$coords)
}

#' @rdname setget_methods
#' @export
get_methodspec<-function(obj)
{
  UseMethod("get_methodspec",obj)
}

#' @rdname setget_methods
#' @export
get_methodspec.default<-function(obj)
{
  stop("Error in get_methodspec: get_methodspec not defined for this class")
}

#' @rdname clust_methods
#' @export
get_methodspec.clust<-function(obj)
{
  return(obj$methodspec)
}

#' @rdname setget_methods
#' @export
get_wmfs<-function(obj)
{
  UseMethod("get_wmfs",obj)
}

#' @rdname setget_methods
#' @export
get_wmfs.default<-function(obj)
{
  stop("Error in get_wmfs: get_wmfs not defined for this class")
}

#' @rdname clust_methods
#' @export
get_wmfs.clust<-function(obj)
{
  return(obj$wmfs)
}

#' @rdname setget_methods
#' @export
get_wpmfs<-function(obj)
{
  UseMethod("get_wpmfs",obj)
}

#' @rdname setget_methods
#' @export
get_wpmfs.default<-function(obj)
{
  stop("Error in get_wpmfs: get_wpmfs not defined for this class")
}

#' @rdname clust_methods
#' @export
get_wpmfs.clust<-function(obj)
{
  return(obj$wpmfs)
}

