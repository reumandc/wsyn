#' Basic methods for the \code{wpmf} class
#' 
#' Set, get, summary, and print methods for the \code{wpmf} class.
#' 
#' @param object,x,obj An object of class \code{wpmf}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.wpmf} produces a summary of a \code{wpmf} object.
#' A \code{print.wpmf} method is also available. For \code{wpmf} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots,
#' i.e., \code{*} equal to \code{times}, \code{timescales}, \code{wtopt}, 
#' \code{values}, \code{dat}, and \code{signif}. The \code{set_*} methods just throw an 
#' error, to prevent breaking the consistency between the slots of a 
#' \code{wpmf} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wpmf}}
#' 
#' @examples
#' times<-1:30 #generate time steps
#' #generate fake count data for 20 locations
#' dat<-matrix(rpois(20*length(times),20),nrow=20,ncol=length(times)) 
#' dat<-cleandat(dat=dat,times=times,clev=2)$cdat #detrend and demean
#' h<-wpmf(dat,times)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name wpmf_methods
NULL
#> NULL

#' @rdname wpmf_methods
#' @export
summary.wpmf<-function(object,...)
{
  x<-object
  
  h<-x$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  res<-list(class="wpmf",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            sampling_locs=dim(x$dat)[1],
            timescale_start=x$timescales[1],
            timescale_end=x$timescales[length(x$timescales)],
            timescale_length=length(x$timescales),
            scale.min=x$wtopt$scale.min,
            scale.max.input=h,
            sigma=x$wtopt$sigma,
            f0=x$wtopt$f0,
            signif_testing=x$signif[[1]])
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname wpmf_methods
#' @export
print.wpmf<-function(x,...)
{
  cat("wpmf object:\n")
  
  cat("times, a length",length(x$times),"numeric vector:\n")
  if (length(x$times)<12)
  {
    cat(paste(x$times),"\n")  
  }else
  {
    cat(paste(x$times[1:5]),"...",paste(x$times[(length(x$times)-4):(length(x$times))]),"\n")
  }
  
  cat("Number of sampling locations:",dim(x$dat)[1],"\n")
  
  cat("timescales, a length",length(x$timescales),"numeric vector:\n")
  if (length(x$timescales)<12)
  {
    cat(paste(x$timescales),"\n")  
  }else
  {
    cat(paste(x$timescales[1:5]),"...",paste(x$timescales[(length(x$timescales)-4):(length(x$timescales))]),"\n")
  }
  
  if (length(x$timescales)<=5 && length(x$times)<=5)
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2]," matrix, to four digits:\n")
    print(round(x$values,4))
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left to four digits is:\n")
    print(round(x$values[1:5,1:5],4))
  }
  
  w<-x$wtopt
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,"\n",sep="")
  
  if (inherits(x$signif,"list"))
  {
    cat("significance testing:",x$signif[[1]])
  }else
  {
    cat("significance testing: NA")
  }
}

#' @rdname wpmf_methods
#' @export
set_times.wpmf<-function(obj,newval)
{
  stop("Error in set_times: times scould not be altered for a wpmf object")
}

#' @rdname wpmf_methods
#' @export
set_timescales.wpmf<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a wpmf object")
}

#' @rdname wpmf_methods
#' @export
set_values.wpmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wpmf object")
}

#' @rdname wpmf_methods
#' @export
set_dat.wpmf<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wpmf object")
}

#' @rdname wpmf_methods
#' @export
set_wtopt.wpmf<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wpmf object")
}

#' @rdname setget_methods
#' @export
set_signif<-function(obj,newval)
{
  UseMethod("set_signif",obj)  
}

#' @rdname setget_methods
#' @export
set_signif.default<-function(obj,newval)
{
  stop("Error in set_signif: set_signif not defined for this class")
}

#' @rdname wpmf_methods
#' @export
set_signif.wpmf<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a wpmf object")
}

#' @rdname wpmf_methods
#' @export
get_times.wpmf<-function(obj)
{
  return(obj$times)
}

#' @rdname wpmf_methods
#' @export
get_timescales.wpmf<-function(obj)
{
  return(obj$timescales)
}

#' @rdname wpmf_methods
#' @export
get_values.wpmf<-function(obj)
{
  return(obj$values)
}

#' @rdname wpmf_methods
#' @export
get_dat.wpmf<-function(obj)
{
  return(obj$dat)
}

#' @rdname wpmf_methods
#' @export
get_wtopt.wpmf<-function(obj)
{
  return(obj$wtopt)
}

#' @rdname setget_methods
#' @export
get_signif<-function(obj)
{
  UseMethod("get_signif",obj)
}

#' @rdname setget_methods
#' @export
get_signif.default<-function(obj)
{
  stop("Error in get_signif: get_signif not defined for this class")
}

#' @rdname wpmf_methods
#' @export
get_signif.wpmf<-function(obj)
{
  return(obj$signif)
}



