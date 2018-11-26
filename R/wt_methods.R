#Simple methods of the wt class

#value setting - these just throw an error, since we do not want
#individual components of a wt object changed as that breaks the
#consistency among the components

#' @export
set_times.wt<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wt object")
}

#' @export
set_wtopt<-function(obj,newval)
{
  UseMethod("set_wtopt",obj)
}

#' @export
set_wtopt.default<-function(obj,newval)
{
  stop("Error in set_wtopt: set_wtopt not defined for this class")
}

#' @export
set_wtopt.wt<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wt object")
}

#' @export
set_timescales.wt<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be alterned for a wt object")
}

#' @export
set_values.wt<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wt object")
}

#' @export
set_dat.wt<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wt object")
}

#value getting - methods not needed except for dat, others inherited from tts

#' @export
get_dat.wt<-function(obj)
{
  return(obj$dat)
}

#' @export
get_wtopt<-function(obj)
{
  UseMethod("get_wtopt",obj)
}

#' @export
get_wtopt.default<-function(obj)
{
  stop("Error in get_wtopt: get_wtopt not defined for this class")
}

#' @export
get_wtopt.wt<-function(obj)
{
  return(obj$wtopt)
}

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
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2]," matrix:\n")
    print(x$values)
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left is:\n")
    print(x$values[1:5,1:5])
  }
  
  w<-x$wtopt
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,sep="")
}

#write a summary method when the time comes

