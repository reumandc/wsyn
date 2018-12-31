#' Basic methods for the \code{coh} class
#' 
#' Set, get, summary, and print methods for the \code{coh} class.
#' 
#' @param object,x,obj An object of class \code{coh}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.coh} produces a summary of a \code{coh} object.
#' A \code{print.coh} method is also available. For \code{coh} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{coh} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{coh} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{coh}}
#' 
#' @examples
#' times<-1:100
#' dat1<-matrix(rnorm(1000),10,100)
#' dat2<-matrix(rnorm(1000),10,100)
#' dat1<-cleandat(dat1,times,1)$cdat
#' dat2<-cleandat(dat2,times,1)$cdat
#' norm<-"powall"
#' sigmethod<-"fast"
#' nrand<-10
#' h<-coh(dat1,dat2,times,norm,sigmethod,nrand)
#' get_times(h)
#' summary(h)
#' print(h)
#' 
#' @name coh_methods
NULL
#> NULL

#' @rdname coh_methods
#' @export
summary.coh<-function(object,...)
{
  x<-object
  
  h<-x$wtopt$scale.max.input
  if (is.null(h)){h<-"NULL"}
  
  #whether the ranks slot is full
  if (class(x$ranks)=="list")
  {
    h2<-"filled"
  }else
  {
    h2<-"empty"
  }
  
  res<-list(class="coh",
            times_start=x$times[1],
            times_end=x$times[length(x$times)],
            times_increment=x$times[2]-x$times[1],
            sampling_locs=dim(x$dat1)[1],
            timescale_start=x$timescales[1],
            timescale_end=x$timescales[length(x$timescales)],
            timescale_length=length(x$timescales),
            normalization=x$norm,
            scale.min=x$wtopt$scale.min,
            scale.max.input=h,
            sigma=x$wtopt$sigma,
            f0=x$wtopt$f0,
            sigmethod=x$sigmethod,
            ranks_slot_is=h2)
  
  #a summary_wsyn object inherits from the list class, but has its own print method, above
  class(res)<-c("summary_wsyn","list")
  return(res)
}

#' @rdname coh_methods
#' @export
print.coh<-function(x,...)
{
  cat("coh object:\n")
  
  cat("times, a length",length(x$times),"numeric vector:\n")
  if (length(x$times)<12)
  {
    cat(paste(x$times),"\n")  
  }else
  {
    cat(paste(x$times[1:5]),"...",paste(x$times[(length(x$times)-4):(length(x$times))]),"\n")
  }
  
  cat("Number of sampling locations:",dim(x$dat1)[1],"\n")
  
  cat("timescales, a length",length(x$timescales),"numeric vector:\n")
  if (length(x$timescales)<12)
  {
    cat(paste(x$timescales),"\n")  
  }else
  {
    cat(paste(x$timescales[1:5]),"...",paste(x$timescales[(length(x$timescales)-4):(length(x$timescales))]),"\n")
  }
  
  cat("norm, the normalization used:",x$norm,"\n")
  
  w<-x$wtopt
  if (is.null(w$scale.max.input)){w$scale.max.input<-"NULL"}
  cat("wtopt: scale.min=",w$scale.min,"; scale.max.input=",w$scale.max.input,"; sigma=",w$sigma,"; f0=",w$f0,"\n",sep="")
  
  cat("sigmethod, the type of significance testing used:",x$sigmethod,"\n")
  
  if (class(x$signif)=="list")
  {
    cat("Number of surrogates:",dim(x$signif$scoher)[1],"\n")
  }else
  {
    cat("Number of surrogates: NA\n")
  }
  
  if (class(x$ranks)=="list")
  {
    cat("The ranks slot is: filled\n")
  }else
  {
    cat("The ranks slot is: empty\n")  
  }
  
  if (class(x$bandp)=="data.frame")
  {
    cat("Timescale bands tested in bandp slot:\n")
    h<-print(x$bandp[,c(1,2)])
  }else
  {
    cat("Timescale bands tested in bandp slot: none")
  }
}

#' @rdname coh_methods
#' @export
set_times.coh<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a coh object")
}

#' @rdname coh_methods
#' @export
set_timescales.coh<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_coher<-function(obj,newval)
{
  UseMethod("set_coher",obj)
}

#' @rdname setget_methods
#' @export
set_coher.default<-function(obj,newval)
{
  stop("Error in set_coher: set_coher not defined for this class")
}

#' @rdname coh_methods
#' @export
set_coher.coh<-function(obj,newval)
{
  stop("Error in set_coher: coher should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_dat1<-function(obj,newval)
{
  UseMethod("set_dat1",obj)
}

#' @rdname setget_methods
#' @export
set_dat1.default<-function(obj,newval)
{
  stop("Error in set_dat1: set_dat1 not defined for this class")
}

#' @rdname coh_methods
#' @export
set_dat1.coh<-function(obj,newval)
{
  stop("Error in set_dat1: dat1 should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_dat2<-function(obj,newval)
{
  UseMethod("set_dat2",obj)
}

#' @rdname setget_methods
#' @export
set_dat2.default<-function(obj,newval)
{
  stop("Error in set_dat2: set_dat2 not defined for this class")
}

#' @rdname coh_methods
#' @export
set_dat2.coh<-function(obj,newval)
{
  stop("Error in set_dat2: dat2 should not be altered for a coh object")
}

#' @rdname coh_methods
#' @export
set_wtopt.coh<-function(obj,newval)
{
  stop("Error in set_wtopt: wtopt should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_norm<-function(obj,newval)
{
  UseMethod("set_norm",obj)
}

#' @rdname setget_methods
#' @export
set_norm.default<-function(obj,newval)
{
  stop("Error in set_norm: set_norm not defined for this class")
}

#' @rdname coh_methods
#' @export
set_norm.coh<-function(obj,newval)
{
  stop("Error in set_norm: norm should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_sigmethod<-function(obj,newval)
{
  UseMethod("set_sigmethod",obj)
}

#' @rdname setget_methods
#' @export
set_sigmethod.default<-function(obj,newval)
{
  stop("Error in set_sigmethod: set_sigmethod not defined for this class")
}

#' @rdname coh_methods
#' @export
set_sigmethod.coh<-function(obj,newval)
{
  stop("Error in set_sigmethod: sigmethod should not be altered for a coh object")
}

#' @rdname coh_methods
#' @export
set_signif.coh<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_ranks<-function(obj,newval)
{
  UseMethod("set_ranks",obj)
}

#' @rdname setget_methods
#' @export
set_ranks.default<-function(obj,newval)
{
  stop("Error in set_ranks: set_ranks not defined for this class")
}

#' @rdname coh_methods
#' @export
set_ranks.coh<-function(obj,newval)
{
  stop("Error in set_ranks: ranks should not be altered for a coh object")
}

#' @rdname setget_methods
#' @export
set_bandp<-function(obj,newval)
{
  UseMethod("set_bandp",obj)
}

#' @rdname setget_methods
#' @export
set_bandp.default<-function(obj,newval)
{
  stop("Error in set_bandp: set_bandp not defined for this class")
}

#' @rdname coh_methods
#' @export
set_bandp.coh<-function(obj,newval)
{
  stop("Error in set_bandp: bandp should not be altered for a coh object")
}

#' @rdname coh_methods
#' @export
get_times.coh<-function(obj)
{
  return(obj$times)
}

#' @rdname coh_methods
#' @export
get_timescales.coh<-function(obj)
{
  return(obj$timescales)
}

#' @rdname setget_methods
#' @export
get_coher<-function(obj)
{
  UseMethod("get_coher",obj)
}

#' @rdname setget_methods
#' @export
get_coher.default<-function(obj)
{
  stop("Error in get_coher: get_coher not defined for this class")
}

#' @rdname coh_methods
#' @export
get_coher.coh<-function(obj)
{
  return(obj$coher)
}

#' @rdname setget_methods
#' @export
get_dat1<-function(obj)
{
  UseMethod("get_dat1",obj)
}

#' @rdname setget_methods
#' @export
get_dat1.default<-function(obj)
{
  stop("Error in get_dat1: get_dat1 not defined for this class")
}

#' @rdname coh_methods
#' @export
get_dat1.coh<-function(obj)
{
  return(obj$dat1)
}

#' @rdname setget_methods
#' @export
get_dat2<-function(obj)
{
  UseMethod("get_dat2",obj)
}

#' @rdname setget_methods
#' @export
get_dat2.default<-function(obj)
{
  stop("Error in get_dat2: get_dat2 not defined for this class")
}

#' @rdname coh_methods
#' @export
get_dat2.coh<-function(obj)
{
  return(obj$dat2)
}

#' @rdname coh_methods
#' @export
get_wtopt.coh<-function(obj)
{
  return(obj$wtopt)
}

#' @rdname setget_methods
#' @export
get_norm<-function(obj)
{
  UseMethod("get_norm",obj)
}

#' @rdname setget_methods
#' @export
get_norm.default<-function(obj)
{
  stop("Error in get_norm: get_norm not defined for this class")
}

#' @rdname coh_methods
#' @export
get_norm.coh<-function(obj)
{
  return(obj$norm)
}

#' @rdname setget_methods
#' @export
get_sigmethod<-function(obj)
{
  UseMethod("get_sigmethod",obj)
}

#' @rdname setget_methods
#' @export
get_sigmethod.default<-function(obj)
{
  stop("Error in get_sigmethod: get_sigmethod not defined for this class")
}

#' @rdname coh_methods
#' @export
get_sigmethod.coh<-function(obj)
{
  return(obj$sigmethod)
}

#' @rdname coh_methods
#' @export
get_signif.coh<-function(obj)
{
  return(obj$signif)
}

#' @rdname setget_methods
#' @export
get_ranks<-function(obj)
{
  UseMethod("get_ranks",obj)
}

#' @rdname setget_methods
#' @export
get_ranks.default<-function(obj)
{
  stop("Error in get_ranks: get_ranks not defined for this class")
}

#' @rdname coh_methods
#' @export
get_ranks.coh<-function(obj)
{
  return(obj$ranks)
}

#' @rdname setget_methods
#' @export
get_bandp<-function(obj)
{
  UseMethod("get_bandp",obj)
}

#' @rdname setget_methods
#' @export
get_bandp.default<-function(obj)
{
  stop("Error in get_bandp: get_bandp not defined for this class")
}

#' @rdname coh_methods
#' @export
get_bandp.coh<-function(obj)
{
  return(obj$bandp)
}

