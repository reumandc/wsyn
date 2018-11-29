#Simple methods for the tts class

#value setting

#' @export
set_times<-function(obj,newval)
{
  UseMethod("set_times",obj)
}

#' @export
set_times.default<-function(obj,newval)
{
  stop("Error in set_times: set_times not defined for this class")
}

#' @export
set_times.tts<-function(obj,newval)
{
  errcheck_tts(newval,obj$timescales,obj$values,"set_times.tts")
  obj$times<-newval
  return(obj)
}

#' @export
set_timescales<-function(obj,newval)
{
  UseMethod("set_timescales",obj)
}

#' @export
set_timescales.default<-function(obj,newval)
{
  stop("Error in set_timescales: set_timescales not defined for this class")
}

#' @export
set_timescales.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,newval,obj$values,"set_timescales.tts")
  obj$timescales<-newval
  return(obj)
}

#' @export
set_values<-function(obj,newval)
{
  UseMethod("set_values",obj)
}

#' @export
set_values.default<-function(obj,newval)
{
  stop("Error in set_values: set_values not defined for this class")
}

#' @export
set_values.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,obj$timescales,newval,"set_values.tts")
  obj$values<-newval
  return(obj)
}

#value getting

#' @export
get_times<-function(obj)
{
  UseMethod("get_times",obj)
}

#' @export
get_times.default<-function(obj)
{
  stop("Error in get_times: get_times not defined for this class")
}

#' @export
get_times.tts<-function(obj)
{
  return(obj$times)
}

#' @export
get_timescales<-function(obj)
{
  UseMethod("get_timescales",obj)
}

#' @export
get_timescales.default<-function(obj)
{
  stop("Error in get_timescales: get_timescales not defined for this class")
}

#' @export
get_timescales.tts<-function(obj)
{
  return(obj$timescales)
}

#' @export
get_values<-function(obj)
{
  UseMethod("get_values",obj)
}

#' @export
get_values.default<-function(obj)
{
  stop("Error in get_values: get_values not defined for this class")
}

#' @export
get_values.tts<-function(obj)
{
  return(obj$values)
}

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
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix:\n")
    print(x$values)
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left is:\n")
    print(x$values[1:5,1:5])
  }
}

#' @export
print.summary_wsyn<-function(x,...)
{
  for (counter in 1:length(x))
  {
    cat(names(x)[counter],": ",x[[counter]],"\n",sep="")
  }
}

#' @export
summary.tts<-function(x,...)
{
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