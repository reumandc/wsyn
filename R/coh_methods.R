#Simple methods for the class coh

#value setting
set_dat2<-function(obj,newval)
{
  UseMethod("set_dat2",obj)
}

set_dat2.default<-function(obj,newval)
{
  stop("Error in set_dat2: set_dat2 not defined for this class")
}

set_dat2.coh<-function(obj,newval)
{
  stop("Error in set_dat2: dat2 should not be altered for a coh object")
}

set_dat1<-function(obj,newval)
{
  UseMethod("set_dat1",obj)
}

set_dat1.default<-function(obj,newval)
{
  stop("Error in set_dat1: set_dat1 not defined for this class")
}

set_dat1.coh<-function(obj,newval)
{
  stop("Error in set_dat1: dat1 should not be altered for a coh object")
}

set_times.coh<-function(obj,newval)
{
  stop("Error in set_times: times should not be altered for a coh object")
}

set_sigmethod<-function(obj,newval)
{
  UseMethod("set_sigmethod",obj)
}

set_sigmethod.default<-function(obj,newval)
{
  stop("Error in set_sigmethod: set_sigmethod not defined for this class")
}

set_sigmethod.coh<-function(obj,newval)
{
  stop("Error in set_sigmethod: sigmethod should not be altered for a coh object")
}

set_norm<-function(obj,newval)
{
  UseMethod("set_norm",obj)
}

set_norm.default<-function(obj,newval)
{
  stop("Error in set_norm: set_norm not defined for this class")
}

set_norm.coh<-function(obj,newval)
{
  stop("Error in set_norm: norm should not be altered for a coh object")
}

set_timescales.coh<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a coh object")
}

set_coher<-function(obj,newval)
{
  UseMethod("set_coher",obj)
}

set_coher.default<-function(obj,newval)
{
  stop("Error in set_coher: set_coher not defined for this class")
}

set_coher.coh<-function(obj,newval)
{
  stop("Error in set_coher: coher should not be altered for a coh object")
}

set_signif.coh<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a coh object")
}

set_bandp<-function(obj,newval)
{
  UseMethod("set_bandp",obj)
}

set_bandp.default<-function(obj,newval)
{
  stop("Error in set_bandp: set_bandp not defined for this class")
}

set_bandp.coh<-function(obj,newval)
{
  stop("Error in set_bandp: bandp should not be altered for a coh object")
}

#value getting
get_dat1<-function(obj)
{
  UseMethod("get_dat1",obj)
}

get_dat1.default<-function(obj)
{
  stop("Error in get_dat1: get_dat1 not defined for this class")
}

get_dat1.coh<-function(obj)
{
  return(obj$dat1)
}

get_dat2<-function(obj)
{
  UseMethod("get_dat2",obj)
}

get_dat2.default<-function(obj)
{
  stop("Error in get_dat2: get_dat2 not defined for this class")
}

get_dat2.coh<-function(obj)
{
  return(obj$dat2)
}

get_times.coh<-function(obj)
{
  return(obj$times)
}

get_sigmethod<-function(obj)
{
  UseMethod("get_sigmethod",obj)
}

get_sigmethod.default<-function(obj)
{
  stop("Error in get_sigmethod: get_sigmethod not defined for this class")
}

get_sigmethod.coh<-function(obj)
{
  return(obj$sigmethod)
}

get_norm<-function(obj)
{
  UseMethod("get_norm",obj)
}

get_norm.default<-function(obj)
{
  stop("Error in get_norm: get_norm not defined for this class")
}

get_norm.coh<-function(obj)
{
  return(obj$norm)
}

get_timescales.coh<-function(obj)
{
  return(obj$timescales)
}

get_coher<-function(obj)
{
  UseMethod("get_coher",obj)
}

get_coher.default<-function(obj)
{
  stop("Error in get_coher: get_coher not defined for this class")
}

get_coher.coh<-function(obj)
{
  return(obj$coher)
}

get_signif.coh<-function(obj)
{
  return(obj$signif)
}

get_bandp<-function(obj)
{
  UseMethod("get_bandp",obj)
}

get_bandp.default<-function(obj)
{
  stop("Error in get_bandp: get_bandp not defined for this class")
}

get_bandp.coh<-function(obj)
{
  return(obj$bandp)
}

#need summary and print method later