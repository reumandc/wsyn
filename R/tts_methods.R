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

#This is based on an existing generic so not sure if this will work like this
#summary.tts<-function(obj)
#{
#  
#}

#This is based on an existing generic so not sure if this will work like this
#print.tts<-function(obj)
#{
#  
#}

#What else? 
