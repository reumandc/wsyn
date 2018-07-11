#Simple methods for the wlm class

#value setting - these just throw an error, since we do not want
#individual components of a wlm object changed as that breaks the
#consistency among the components

#' @export
set_dat.wlm<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wlm object")
}

#' @export
set_times.wlm<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wlm object")
}

#' @export
set_norm.wlm<-function(obj,newval)
{
  stop("Error in set_norm: norm should not be altered for a wlm object")
}

#' @export
set_wts<-function(obj,newval)
{
  UseMethod("set_wts",obj)
}

#' @export
set_wts.default<-function(obj,newval)
{
  stop("Error in set_wts: set_wts not defined for this class")
}

#' @export
set_wts.wlm<-function(obj,newval)
{
  stop("Error in set_wts: wts should not be altered for a wlm object")
}

#' @export
set_timescales.wlm<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a wlm object")
}

#' @export
set_coefs<-function(obj,newval)
{
  UseMethod("set_coefs",obj)
}

#' @export
set_coefs.default<-function(obj,newval)
{
  stop("Error in set_coefs: set_coefs not defined for this class")
}

#' @export
set_coefs.wlm<-function(obj,newval)
{
  stop("Error in set_coefs: coefs should not be altered for a wlm object")
}

#' @export
set_modval<-function(obj,newval)
{
  UseMethod("set_modval",obj)
}

#' @export
set_modval.default<-function(obj,newval)
{
  stop("Error in set_modval: set_modval not defined for this class")
}

#' @export
set_modval.wlm<-function(obj,newval)
{
  stop("Error in set_modval: modval should not be altered for a wlm object")
}

#' @export
set_coher.wlm<-function(obj,newval)
{
  stop("Error in set_coher: coher should not be altered for a wlm object")
}

#get methods

#' @export
get_dat.wlm<-function(obj)
{
  return(obj$dat)
}

#' @export
get_times.wlm<-function(obj)
{
  return(obj$times)
}

#' @export
get_norm.wlm<-function(obj)
{
  return(obj$norm)
}

#' @export
get_wts<-function(obj)
{
  UseMethod("get_wts",obj)
}

#' @export
get_wts.default<-function(obj)
{
  stop("Error in get_wts: get_wts not defined for this class")
}

#' @export
get_wts.wlm<-function(obj)
{
  return(obj$wts)
}

#' @export
get_timescales.wlm<-function(obj)
{
  return(obj$timescales)
}

#' @export
get_coefs<-function(obj)
{
  UseMethod("get_coefs",obj)
}

#' @export
get_coefs.default<-function(obj)
{
  stop("Error in get_coefs: get_coefs not defined for this class")
}

#' @export
get_coefs.wlm<-function(obj)
{
  return(obj$coefs)
}

#' @export
get_modval<-function(obj)
{
  UseMethod("get_modval",obj)
}

#' @export
get_modval.default<-function(obj)
{
  stop("Error in get_modval: get_modval not defined for this class")
}

#' @export
get_modval.wlm<-function(obj)
{
  return(obj$modval)
}

#' @export
get_coher.wlm<-function(obj)
{
  return(obj$coher)
}

#Need print and summary methods