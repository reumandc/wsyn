#' Basic methods for the \code{wmf} class
#' 
#' Set, get, summary, and print methods for the \code{wmf} class.
#' 
#' @param object,x,obj An object of class \code{wmf}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.wmf} produces a summary of a \code{wmf} object.
#' A \code{print.wmf} method is also available. For \code{wmf} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots,
#' i.e., \code{*} equal to \code{times}, \code{timescales}, \code{wtopt}, 
#' \code{values}, and \code{dat}. The \code{set_*} methods just throw an 
#' error, to prevent breaking the consistency between the slots of a 
#' \code{wmf} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wmf}}
#' 
#' @examples
#' times<-1:30 #generate time steps
#' #generate fake count data for 20 locations
#' dat<-matrix(rpois(20*length(times),20),nrow=20,ncol=length(times)) 
#' dat<-cleandat(dat=dat,times=times,clev=2)$cdat #detrend and demean
#' h<-wmf(dat,times)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name wmf_methods
NULL
#> NULL

#' @rdname wmf_methods
#' @export
summary.wmf<-function(object,...)
{
  x<-object
  
  h<-x$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  res<-list(class="wmf",
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
            f0=x$wtopt$f0)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname wmf_methods
#' @export
print.wmf<-function(x,...)
{
  cat("wmf object:\n")
  
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
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,sep="")
}

#' @rdname wmf_methods
#' @export
set_times.wmf<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wmf object")
}

#' @rdname wmf_methods
#' @export
set_timescales.wmf<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be alterned for a wmf object")
}

#' @rdname wmf_methods
#' @export
set_values.wmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wmf object")
}

#' @rdname wmf_methods
#' @export
set_dat.wmf<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wmf object")
}

#' @rdname wmf_methods
#' @export
set_wtopt.wmf<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wmf object")
}

#' @rdname wmf_methods
#' @export
get_times.wmf<-function(obj)
{
  return(obj$times)
}

#' @rdname wmf_methods
#' @export
get_timescales.wmf<-function(obj)
{
  return(obj$timescales)
}

#' @rdname wmf_methods
#' @export
get_values.wmf<-function(obj)
{
  return(obj$values)
}

#' @rdname wmf_methods
#' @export
get_dat.wmf<-function(obj)
{
  return(obj$dat)
}

#' @rdname wmf_methods
#' @export
get_wtopt.wmf<-function(obj)
{
  return(obj$wtopt)
}

