#Simple methods of the wpmf class

#value setting - these just throw an error, since we do not want
#individual components of a wpmf object changed as that breaks the
#consistency among the components

#' @export
set_times.wpmf<-function(obj,newval)
{
  stop("Error in set_times: times scould not be altered for a wpmf object")
}

#' @export
set_timescales.wpmf<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a wpmf object")
}

#' @export
set_values.wpmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wpmf object")
}

#' @export
set_dat<-function(obj,newval)
{
  UseMethod("set_dat",obj)
}

#' @export
set_dat.default<-function(obj,newval)
{
  stop("Error in set_dat: set_dat not defined for this class")
}

#' @export
set_dat.wpmf<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wpmf object")
}

#' @export
set_signif<-function(obj,newval)
{
  UseMethod("set_signif",obj)  
}

#' @export
set_signif.default<-function(obj,newval)
{
  stop("Error in set_signif: set_signif not defined for this class")
}

#' @export
set_signif.wpmf<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a wpmf object")
}

#' @export
set_wtopt.wpmf<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wpmf object")
}

#value getting - methods not needed except for dat, others inherited from tts

#' @export
get_dat<-function(obj)
{
  UseMethod("get_dat",obj)
}

#' @export
get_dat.default<-function(obj)
{
  stop("Error in get_dat: get_dat not defined for this class")
}

#' @export
get_dat.wpmf<-function(obj)
{
  return(obj$dat)
}

#' @export
get_signif<-function(obj)
{
  UseMethod("get_signif",obj)
}

#' @export
get_signif.default<-function(obj)
{
  stop("Error in get_signif: get_signif not defined for this class")
}

#' @export
get_signif.wpmf<-function(obj)
{
  return(obj$signif)
}

#' @export
get_wtopt.wpmf<-function(obj)
{
  return(obj$wtopt)
}

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
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2]," matrix:\n")
    print(x$values)
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left is:\n")
    print(x$values[1:5,1:5])
  }
  
  w<-x$wtopt
  cat("wtopt:\n")
  cat("scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,"\n",sep="")
  
  if (class(x$signif)=="list")
  {
    cat("significance testing:",x$signif[[1]])
  }else
  {
    cat("significance testing: NA")
  }
}

#write a summary method when the time comes

