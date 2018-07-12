#Simple methods of the wt class

#value setting - these just throw an error, since we do not want
#individual components of a wlmtest object changed as that breaks the
#consistency among the components

#' @export
set_wlmobj<-function(obj,newval)
{
  UseMethod("set_wlmobj",obj)
}

#' @export
set_wlmobj.default<-function(obj,newval)
{
  stop("Error in set_wlmobj: set_wlmobj not defined for this class")
}

#' @export
set_wlmobj.wlmtest<-function(obj,newval)
{
  stop("Error in set_wlmobj: wlmobj should not be altered for a wlmtest object")
}

#' @export
set_drop<-function(obj,newval)
{
  UseMethod("set_drop",obj)
}

#' @export
set_drop.default<-function(obj,newval)
{
  stop("Error in set_drop: set_drop not defined for this class")
}

#' @export
set_drop.wlmtest<-function(obj,newval)
{
  stop("Error in set_drop:drop should not be altered for a wlmtest object")
}

#' @export
set_signif.wlmtest<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a wlmtest object")
}

#' @export
set_ranks.wlmtest<-function(obj,newval)
{
  stop("Error in set_ranks: ranks should not be altered for a wlmtest object")
}

#' @export
set_bandp.wlmtest<-function(obj,newval)
{
  stop("Error in set_bandp: bandp should not be altered for a wlmtest object")
}

#get methods

#' @export
get_wlmobj<-function(obj)
{
  UseMethod("get_wlmobj",obj)
}

#' @export
get_wlmobj.default<-function(obj)
{
  stop("Error in get_wlmobj: get_wlmobj not defined for this class")
}

#' @export
get_wlmobj.wlmtest<-function(obj)
{
  return(obj$wlmobj)
}

#' @export
get_drop<-function(obj)
{
  UseMethod("get_drop",obj)
}

#' @export
get_drop.default<-function(obj)
{
  stop("Error in get_drop: get_drop not defined for this class")
}

#' @export
get_drop.wlmtest<-function(obj)
{
  return(obj$drop)
}

#' @export
get_signif.wlmtest<-function(obj)
{
  return(obj$signif)
}

#' @export
get_ranks.wlmtest<-function(obj)
{
  return(obj$ranks)
}

#' @export
get_bandp.wlmtest<-function(obj)
{
  return(obj$bandp)
}

#add print and summary methods when the time comes