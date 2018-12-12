#' Basic methods for the \code{wlm} class
#' 
#' Set, get, summary, and print methods for the \code{wlm} class.
#' 
#' @param object,x,obj An object of class \code{wlm}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.wlm} produces a summary of a \code{wlm} object.
#' A \code{print.wlm} method is also available. For \code{wlm} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{wlm} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{wlm} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wlm}}
#' 
#' @examples
#' times<-1:30
#' dat<-list(v1=matrix(rnorm(300),10,30),v2=matrix(rnorm(300),10,30),v3=matrix(rnorm(300),10,30),
#'           v4=matrix(rnorm(300),10,30),v5=matrix(rnorm(300),10,30))
#' dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
#' resp<-2
#' pred<-c(1,3,4)
#' norm<-"powall"
#' h<-wlm(dat,times,resp,pred,norm)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name wlm_methods
NULL
#> NULL

#' @rdname wlm_methods
#' @export
summary.wlm<-function(object,...)
{
  x<-object
  
  h<-x$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  regform<-paste0(names(x$dat)[1],"~")
  for (counter in 2:length(x$dat))
  {
    regform<-paste0(regform,names(x$dat)[counter])
    if (counter<length(x$dat))
    {
      regform<-paste0(regform,"+")
    }
  }
  
  res<-list(class="wlm",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            sampling_locs=dim(x$dat[[1]])[1],
            timescale_start=x$timescales[1],
            timescale_end=x$timescales[length(x$timescales)],
            timescale_length=length(x$timescales),
            wavelet_regression=regform,
            normalization=x$norm,
            scale.min=x$wtopt$scale.min,
            scale.max.input=h,
            sigma=x$wtopt$sigma,
            f0=x$wtopt$f0)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname wlm_methods
#' @export
print.wlm<-function(x,...)
{
  cat("wlm object:\n")
  
  cat("times, a length",length(x$times),"numeric vector:\n")
  if (length(x$times)<12)
  {
    cat(paste(x$times),"\n")  
  }else
  {
    cat(paste(x$times[1:5]),"...",paste(x$times[(length(x$times)-4):(length(x$times))]),"\n")
  }
  
  cat("Number of sampling locations:",dim(x$dat[[1]])[1],"\n")
  
  cat("timescales, a length",length(x$timescales),"numeric vector:\n")
  if (length(x$timescales)<12)
  {
    cat(paste(x$timescales),"\n")  
  }else
  {
    cat(paste(x$timescales[1:5]),"...",paste(x$timescales[(length(x$timescales)-4):(length(x$timescales))]),"\n")
  }
  
  regform<-paste0(names(x$dat)[1],"~")
  for (counter in 2:length(x$dat))
  {
    regform<-paste0(regform,names(x$dat)[counter])
    if (counter<length(x$dat))
    {
      regform<-paste0(regform,"+")
    }
  }
  cat("The wavelet regression:",regform,"\n")
  
  cat("norm, the normalization used:",x$norm,"\n")
  
  w<-x$wtopt
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,"\n",sep="")
}

#' @rdname wlm_methods
#' @export
set_times.wlm<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
set_timescales.wlm<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a wlm object")
}

#' @rdname setget_methods
#' @export
set_coefs<-function(obj,newval)
{
  UseMethod("set_coefs",obj)
}

#' @rdname setget_methods
#' @export
set_coefs.default<-function(obj,newval)
{
  stop("Error in set_coefs: set_coefs not defined for this class")
}

#' @rdname wlm_methods
#' @export
set_coefs.wlm<-function(obj,newval)
{
  stop("Error in set_coefs: coefs should not be altered for a wlm object")
}

#' @rdname setget_methods
#' @export
set_modval<-function(obj,newval)
{
  UseMethod("set_modval",obj)
}

#' @rdname setget_methods
#' @export
set_modval.default<-function(obj,newval)
{
  stop("Error in set_modval: set_modval not defined for this class")
}

#' @rdname wlm_methods
#' @export
set_modval.wlm<-function(obj,newval)
{
  stop("Error in set_modval: modval should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
set_coher.wlm<-function(obj,newval)
{
  stop("Error in set_coher: coher should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
set_dat.wlm<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
set_wtopt.wlm<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
set_norm.wlm<-function(obj,newval)
{
  stop("Error in set_norm: norm should not be altered for a wlm object")
}

#' @rdname setget_methods
#' @export
set_wts<-function(obj,newval)
{
  UseMethod("set_wts",obj)
}

#' @rdname setget_methods
#' @export
set_wts.default<-function(obj,newval)
{
  stop("Error in set_wts: set_wts not defined for this class")
}

#' @rdname wlm_methods
#' @export
set_wts.wlm<-function(obj,newval)
{
  stop("Error in set_wts: wts should not be altered for a wlm object")
}

#' @rdname wlm_methods
#' @export
get_times.wlm<-function(obj)
{
  return(obj$times)
}

#' @rdname wlm_methods
#' @export
get_timescales.wlm<-function(obj)
{
  return(obj$timescales)
}

#' @rdname setget_methods
#' @export
get_coefs<-function(obj)
{
  UseMethod("get_coefs",obj)
}

#' @rdname setget_methods
#' @export
get_coefs.default<-function(obj)
{
  stop("Error in get_coefs: get_coefs not defined for this class")
}

#' @rdname wlm_methods
#' @export
get_coefs.wlm<-function(obj)
{
  return(obj$coefs)
}

#' @rdname setget_methods
#' @export
get_modval<-function(obj)
{
  UseMethod("get_modval",obj)
}

#' @rdname setget_methods
#' @export
get_modval.default<-function(obj)
{
  stop("Error in get_modval: get_modval not defined for this class")
}

#' @rdname wlm_methods
#' @export
get_modval.wlm<-function(obj)
{
  return(obj$modval)
}

#' @rdname wlm_methods
#' @export
get_coher.wlm<-function(obj)
{
  return(obj$coher)
}

#' @rdname wlm_methods
#' @export
get_dat.wlm<-function(obj)
{
  return(obj$dat)
}

#' @rdname wlm_methods
#' @export
get_wtopt.wlm<-function(obj)
{
  return(obj$wtopt)
}

#' @rdname wlm_methods
#' @export
get_norm.wlm<-function(obj)
{
  return(obj$norm)
}

#' @rdname setget_methods
#' @export
get_wts<-function(obj)
{
  UseMethod("get_wts",obj)
}

#' @rdname setget_methods
#' @export
get_wts.default<-function(obj)
{
  stop("Error in get_wts: get_wts not defined for this class")
}

#' @rdname wlm_methods
#' @export
get_wts.wlm<-function(obj)
{
  return(obj$wts)
}
