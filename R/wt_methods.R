#' Basic methods for the \code{wt} class
#' 
#' Set, get, summary, and print methods for the \code{wt} class.
#' 
#' @param object,x,obj An object of class \code{wt}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.wt} produces a summary of a \code{wt} object.
#' A \code{print.wt} method is also available. For \code{wt} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots,
#' i.e., \code{*} equal to \code{times}, \code{timescales}, \code{wtopt}, 
#' \code{values}, and \code{dat}. The \code{set_*} methods just throw an 
#' error, to prevent breaking the consistency between the slots of a 
#' \code{wt} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wt}}
#' 
#' @examples
#' time1<-1:100
#' time2<-101:200
#' ts1p1<-sin(2*pi*time1/15)
#' ts1p2<-0*time1
#' ts2p1<-0*time2
#' ts2p2<-sin(2*pi*time2/8)
#' ts1<-ts1p1+ts1p2
#' ts2<-ts2p1+ts2p2
#' ts<-c(ts1,ts2)
#' ra<-rnorm(200,mean=0,sd=0.5)
#' t.series<-ts+ra
#' t.series<-t.series-mean(t.series)
#' times<-c(time1,time2)
#' h<-wt(t.series, times)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name wt_methods
NULL
#> NULL

#' @rdname wt_methods
#' @export
summary.wt<-function(object,...)
{
  x<-object
  
  h<-x$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  res<-list(class="wt",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            timescale_start=x$timescales[1],
            timescale_end=x$timescales[length(x$timescales)],
            timescale_length=length(x$timescales),
            scale.min=x$wtopt$scale.min,
            scale.max.input=h,
            sigma=x$wtopt$sigma,
            f0=x$wtopt$f0)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname wt_methods
#' @export
print.wt<-function(x,...)
{
  cat("wt object:\n")
  
  cat("times, a length",length(x$times),"numeric vector:\n")
  if (length(x$times)<12)
  {
    cat(paste(x$times),"\n")  
  }else
  {
    cat(paste(x$times[1:5]),"...",paste(x$times[(length(x$times)-4):(length(x$times))]),"\n")
  }
  
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
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,sep="")
}

#' @rdname wt_methods
#' @export
set_times.wt<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wt object")
}

#' @rdname wt_methods
#' @export
set_timescales.wt<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be alterned for a wt object")
}

#' @rdname wt_methods
#' @export
set_values.wt<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wt object")
}

#' @rdname setget_methods
#' @export
set_dat<-function(obj,newval)
{
  UseMethod("set_dat",obj)
}

#' @rdname setget_methods
#' @export
set_dat.default<-function(obj,newval)
{
  stop("Error in set_dat: set_dat not defined for this class")
}

#' @rdname wt_methods
#' @export
set_dat.wt<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wt object")
}

#' @rdname setget_methods
#' @export
set_wtopt<-function(obj,newval)
{
  UseMethod("set_wtopt",obj)
}

#' @rdname setget_methods
#' @export
set_wtopt.default<-function(obj,newval)
{
  stop("Error in set_wtopt: set_wtopt not defined for this class")
}

#' @rdname wt_methods
#' @export
set_wtopt.wt<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wt object")
}

#' @rdname wt_methods
#' @export
get_times.wt<-function(obj)
{
  return(obj$times)
}

#' @rdname wt_methods
#' @export
get_timescales.wt<-function(obj)
{
  return(obj$timescales)
}

#' @rdname wt_methods
#' @export
get_values.wt<-function(obj)
{
  return(obj$values)
}

#' @rdname setget_methods
#' @export
get_dat<-function(obj)
{
  UseMethod("get_dat",obj)
}

#' @rdname setget_methods
#' @export
get_dat.default<-function(obj)
{
  stop("Error in get_dat: get_dat not defined for this class")
}

#' @rdname wt_methods
#' @export
get_dat.wt<-function(obj)
{
  return(obj$dat)
}

#' @rdname setget_methods
#' @export
get_wtopt<-function(obj)
{
  UseMethod("get_wtopt",obj)
}

#' @rdname setget_methods
#' @export
get_wtopt.default<-function(obj)
{
  stop("Error in get_wtopt: get_wtopt not defined for this class")
}

#' @rdname wt_methods
#' @export
get_wtopt.wt<-function(obj)
{
  return(obj$wtopt)
}

