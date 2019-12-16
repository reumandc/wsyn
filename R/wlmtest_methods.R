#' Basic methods for the \code{wlmtest} class
#' 
#' Set, get, summary, and print methods for the \code{wlmtest} class.
#' 
#' @param object,x,obj An object of class \code{wlmtest}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.wlmtest} produces a summary of a \code{wlmtest} object.
#' A \code{print.wlmtest} method is also available. For \code{wlmtest} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{wlmtest} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{wlmtest} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wlmtest}}
#' 
#' @examples
#' times<-1:30
#' dat<-list(v1=matrix(rnorm(300),10,30),v2=matrix(rnorm(300),10,30),v3=matrix(rnorm(300),10,30),
#'           v4=matrix(rnorm(300),10,30),v5=matrix(rnorm(300),10,30))
#' dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
#' resp<-1
#' pred<-2:3
#' norm<-"powall"
#' wlmobj<-wlm(dat,times,resp,pred,norm)
#' drop<-3
#' sigmethod<-"fft"
#' h<-wlmtest(wlmobj,drop,sigmethod,nrand=10)
#' get_times(get_wlmobj(h))
#' summary(h)
#' print(h)
#' 
#' @name wlmtest_methods
NULL
#> NULL

#' @rdname wlmtest_methods
#' @export
summary.wlmtest<-function(object,...)
{
  x<-object
  
  h<-x$wlmobj$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  regform<-paste0(names(x$wlmobj$dat)[1],"~")
  for (counter in 2:length(x$wlmobj$dat))
  {
    regform<-paste0(regform,names(x$wlmobj$dat)[counter])
    if (counter<length(x$wlmobj$dat))
    {
      regform<-paste0(regform,"+")
    }
  }
  
  #whether the ranks slot is full
  if (inherits(x$ranks,"list"))
  {
    h2<-"filled"
  }else
  {
    h2<-"empty"
  }
  
  if (is.numeric(x$drop))
  {
    h3<-names(x$wlmobj$dat)[x$drop]
  }else
  {
    h3<-x$drop
  }
  
  res<-list(class="wlmtest",
            times_start=x$wlmobj$times[1],
            times_end=x$wlmobj$times[length(x$wlmobj$times)],
            times_increment=x$wlmobj$times[2]-x$wlmobj$times[1],
            sampling_locs=dim(x$wlmobj$dat[[1]])[1],
            timescale_start=x$wlmobj$timescales[1],
            timescale_end=x$wlmobj$timescales[length(x$wlmobj$timescales)],
            timescale_length=length(x$wlmobj$timescales),
            orig_wavelet_regression=regform,
            predictors_dropped=h3,
            normalization=x$wlmobj$norm,
            sigmethod=x$signif$sigmethod,
            nsurrogs=dim(x$signif$scoher)[1],
            scale.min=x$wlmobj$wtopt$scale.min,
            scale.max.input=h,
            sigma=x$wlmobj$wtopt$sigma,
            f0=x$wlmobj$wtopt$f0,
            ranks_slot_is=h2)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname wlmtest_methods
#' @export
print.wlmtest<-function(x,...)
{
  cat("wlmtest object:\n")
  
  cat("wlmobj$times, a length",length(x$wlmobj$times),"numeric vector:\n")
  if (length(x$wlmobj$times)<12)
  {
    cat(paste(x$wlmobj$times),"\n")  
  }else
  {
    cat(paste(x$wlmobj$times[1:5]),"...",paste(x$wlmobj$times[(length(x$wlmobj$times)-4):(length(x$wlmobj$times))]),"\n")
  }
  
  cat("Number of sampling locations:",dim(x$wlmobj$dat[[1]])[1],"\n")
  
  cat("wlmobj$timescales, a length",length(x$wlmobj$timescales),"numeric vector:\n")
  if (length(x$wlmobj$timescales)<12)
  {
    cat(paste(x$wlmobj$timescales),"\n")  
  }else
  {
    cat(paste(x$wlmobj$timescales[1:5]),"...",paste(x$wlmobj$timescales[(length(x$wlmobj$timescales)-4):(length(x$wlmobj$timescales))]),"\n")
  }
  
  regform<-paste0(names(x$wlmobj$dat)[1],"~")
  for (counter in 2:length(x$wlmobj$dat))
  {
    regform<-paste0(regform,names(x$wlmobj$dat)[counter])
    if (counter<length(x$wlmobj$dat))
    {
      regform<-paste0(regform,"+")
    }
  }
  cat("The original wavelet regression:",regform,"\n")
  
  if (is.numeric(x$drop))
  {
    cat("The indices in wlmobj$dat of predictors dropped:",paste(x$drop),"\n")
  }else
  {
    cat("The names of predictors dropped:",paste(x$drop),"\n")
  }
  
  cat("wlmobj$norm, the normalization used:",x$wlmobj$norm,"\n")
  
  cat("sigmethod, the type of significance testing used:",x$signif$sigmethod,"\n")
  
  cat("Number of surrogates:",dim(x$signif$scoher)[1],"\n")
  
  w<-x$wlmobj$wtopt
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,"\n",sep="")
  
  if (inherits(x$ranks,"list"))
  {
    cat("The ranks slot is: filled\n")
  }else
  {
    cat("The ranks slot is: empty\n")  
  }
  
  if (inherits(x$bandp,"data.frame"))
  {
    cat("Timescale bands tested in bandp slot:\n")
    h<-print(x$bandp[,c(1,2)])
  }else
  {
    cat("Timescale bands tested in bandp slot: none")
  }
}

#' @rdname setget_methods
#' @export
set_wlmobj<-function(obj,newval)
{
  UseMethod("set_wlmobj",obj)
}

#' @rdname setget_methods
#' @export
set_wlmobj.default<-function(obj,newval)
{
  stop("Error in set_wlmobj: set_wlmobj not defined for this class")
}

#' @rdname wlmtest_methods
#' @export
set_wlmobj.wlmtest<-function(obj,newval)
{
  stop("Error in set_wlmobj: wlmobj should not be altered for a wlmtest object")
}

#' @rdname setget_methods
#' @export
set_drop<-function(obj,newval)
{
  UseMethod("set_drop",obj)
}

#' @rdname setget_methods
#' @export
set_drop.default<-function(obj,newval)
{
  stop("Error in set_drop: set_drop not defined for this class")
}

#' @rdname wlmtest_methods
#' @export
set_drop.wlmtest<-function(obj,newval)
{
  stop("Error in set_drop:drop should not be altered for a wlmtest object")
}

#' @rdname wlmtest_methods
#' @export
set_signif.wlmtest<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a wlmtest object")
}

#' @rdname wlmtest_methods
#' @export
set_ranks.wlmtest<-function(obj,newval)
{
  stop("Error in set_ranks: ranks should not be altered for a wlmtest object")
}

#' @rdname wlmtest_methods
#' @export
set_bandp.wlmtest<-function(obj,newval)
{
  stop("Error in set_bandp: bandp should not be altered for a wlmtest object")
}

#' @rdname setget_methods
#' @export
get_wlmobj<-function(obj)
{
  UseMethod("get_wlmobj",obj)
}

#' @rdname setget_methods
#' @export
get_wlmobj.default<-function(obj)
{
  stop("Error in get_wlmobj: get_wlmobj not defined for this class")
}

#' @rdname wlmtest_methods
#' @export
get_wlmobj.wlmtest<-function(obj)
{
  return(obj$wlmobj)
}

#' @rdname setget_methods
#' @export
get_drop<-function(obj)
{
  UseMethod("get_drop",obj)
}

#' @rdname setget_methods
#' @export
get_drop.default<-function(obj)
{
  stop("Error in get_drop: get_drop not defined for this class")
}

#' @rdname wlmtest_methods
#' @export
get_drop.wlmtest<-function(obj)
{
  return(obj$drop)
}

#' @rdname wlmtest_methods
#' @export
get_signif.wlmtest<-function(obj)
{
  return(obj$signif)
}

#' @rdname wlmtest_methods
#' @export
get_ranks.wlmtest<-function(obj)
{
  return(obj$ranks)
}

#' @rdname wlmtest_methods
#' @export
get_bandp.wlmtest<-function(obj)
{
  return(obj$bandp)
}
