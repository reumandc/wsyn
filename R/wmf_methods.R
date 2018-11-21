#Simple methods of the wmf class

#value setting - these just throw an error, since we do not want
#individual components of a wmf object changed as that breaks the
#consistency among the components

#' @export
set_times.wmf<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a wmf object")
}

#' @export
set_timescales.wmf<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be alterned for a wmf object")
}

#' @export
set_values.wmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wmf object")
}

#' @export
set_dat.wmf<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wmf object")
}

#' @export
set_wtopt.wmf<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a wmf object")
}

#value getting - methods not needed except for dat, others inherited from tts

#' @export
get_dat.wmf<-function(obj)
{
  return(obj$dat)
}

#' @export
get_wtopt.wmf<-function(obj)
{
  return(obj$wtopt)
}

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
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2]," matrix:\n")
    print(x$values)
  }else
  {
    cat("values, a",dim(x$values)[1],"by",dim(x$values)[2],"matrix, upper left is:\n")
    print(x$values[1:5,1:5])
  }
  
  w<-x$wtopt
  cat("wtopt:\n")
  cat("scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,sep="")
}

#write a summary method when the time comes

