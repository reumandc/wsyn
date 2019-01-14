#' Basic methods for the \code{tts} class
#' 
#' Set, get, summary, and print methods for the \code{tts} class.
#' 
#' @param object,x,obj An object of class \code{tts}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.tts} produces a summary of a \code{tts} object.
#' A \code{print.tts} method is also available. For \code{tts} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots,
#' i.e., \code{*} equal to \code{times}, \code{timescales}, and 
#' \code{values}. The \code{set_*} methods just throw an error. Although
#' class \code{tts} is flexible enough that setting of individual slots
#' could have been allowed, because \code{wt} and other classes are 
#' based on it and because individual slots of those classes should not 
#' be changed, for consistency the same is forced for the \code{tts} 
#' class.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{tts}}
#' 
#' @examples
#' times<-1:10
#' timescales<-1/c(1:10)
#' values<-matrix(1,length(times),length(timescales))
#' h<-tts(times,timescales,values)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name tts_methods
NULL
#> NULL

#' Set and get methods for classes in the \code{wsyn} package
#' 
#' Set and get methods for classes in the \code{wsyn} package. There
#' are methods for each slot of each class, named \code{set_*} and
#' \code{get_*} for \code{*} the slot name. Below are listed function
#' specs for the generics and the default methods.
#' 
#' @param obj An object of one of the classes defined in the package
#' @param newval A newvalue of the slot in question, for the \code{set_*} methods
#' 
#' @return \code{set_*} methods throw an error - setting of individual
#' slots is not allowed, as it breaks consistency with the other slots.
#' \code{get_*} just returns the value in question. 
#' 
#' @details There are methods for the \code{tts}, \code{wt}, \code{wmf},
#' \code{wpmf}, \code{coh}, \code{wlm}, \code{wlmtest}, and \code{clust}
#' classes. See documentation for the generator functions for these classes
#' (which in all cases have the same name as the class) for lists of slots
#' for each class.
#'
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#'  
#' @examples
#' times<-1:10
#' timescales<-1/c(1:10)
#' values<-matrix(1,length(times),length(timescales))
#' h<-tts(times,timescales,values)
#' get_times(h)
#'
#' @name setget_methods
NULL
#> NULL

#' @rdname tts_methods
#' @export
summary.tts<-function(object,...)
{
  x<-object
  
  res<-list(class="tts",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            timescale_start=x$timescales[1],
            timescale_end=x$timescales[length(x$timescales)],
            timescale_length=length(x$timescales))
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname tts_methods
#' @export
print.tts<-function(x,...)
{
  cat("tts object:\n")
  
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
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, to four digits:\n")
    print(round(x$values,4))
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left to four digits is:\n")
    print(round(x$values[1:5,1:5],4))
  }
}

#' @rdname setget_methods
#' @export
set_times<-function(obj,newval)
{
  UseMethod("set_times",obj)
}

#' @rdname setget_methods
#' @export
set_times.default<-function(obj,newval)
{
  stop("Error in set_times: set_times not defined for this class")
}

#' @rdname tts_methods
#' @export
set_times.tts<-function(obj,newval)
{
  errcheck_tts(newval,obj$timescales,obj$values,"set_times.tts")
  obj$times<-newval
  return(obj)
}

#' @rdname setget_methods
#' @export
set_timescales<-function(obj,newval)
{
  UseMethod("set_timescales",obj)
}

#' @rdname setget_methods
#' @export
set_timescales.default<-function(obj,newval)
{
  stop("Error in set_timescales: set_timescales not defined for this class")
}

#' @rdname tts_methods
#' @export
set_timescales.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,newval,obj$values,"set_timescales.tts")
  obj$timescales<-newval
  return(obj)
}

#' @rdname setget_methods
#' @export
set_values<-function(obj,newval)
{
  UseMethod("set_values",obj)
}

#' @rdname setget_methods
#' @export
set_values.default<-function(obj,newval)
{
  stop("Error in set_values: set_values not defined for this class")
}

#' @rdname tts_methods
#' @export
set_values.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,obj$timescales,newval,"set_values.tts")
  obj$values<-newval
  return(obj)
}

#' @rdname setget_methods
#' @export
get_times<-function(obj)
{
  UseMethod("get_times",obj)
}

#' @rdname setget_methods
#' @export
get_times.default<-function(obj)
{
  stop("Error in get_times: get_times not defined for this class")
}

#' @rdname tts_methods
#' @export
get_times.tts<-function(obj)
{
  return(obj$times)
}

#' @rdname setget_methods
#' @export
get_timescales<-function(obj)
{
  UseMethod("get_timescales",obj)
}

#' @rdname setget_methods
#' @export
get_timescales.default<-function(obj)
{
  stop("Error in get_timescales: get_timescales not defined for this class")
}

#' @rdname tts_methods
#' @export
get_timescales.tts<-function(obj)
{
  return(obj$timescales)
}

#' @rdname setget_methods
#' @export
get_values<-function(obj)
{
  UseMethod("get_values",obj)
}

#' @rdname setget_methods
#' @export
get_values.default<-function(obj)
{
  stop("Error in get_values: get_values not defined for this class")
}

#' @rdname tts_methods
#' @export
get_values.tts<-function(obj)
{
  return(obj$values)
}




