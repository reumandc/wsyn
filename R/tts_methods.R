#Simple methods for the tts class

#value setting
set_times<-function(obj,newval)
{
  UseMethod("set_times",obj)
}

set_times.default<-function(obj,newval)
{
  stop("Error in set_times: set_times not defined for this class")
}

set_times.tts<-function(obj,newval)
{
  errcheck_tts(newval,obj$timescales,obj$values,"set_times.tts")
  obj$times<-newval
  return(obj)
}

set_timescales<-function(obj,newval)
{
  UseMethod("set_timescales",obj)
}

set_timescales.default<-function(obj,newval)
{
  stop("Error in set_timescales: set_timescales not defined for this class")
}

set_timescales.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,newval,obj$values,"set_timescales.tts")
  obj$timescales<-newval
  return(obj)
}

set_values<-function(obj,newval)
{
  UseMethod("set_values",obj)
}

set_values.default<-function(obj,newval)
{
  stop("Error in set_values: set_values not defined for this class")
}

set_values.tts<-function(obj,newval)
{
  errcheck_tts(obj$times,obj$timescales,newval,"set_values.tts")
  obj$values<-newval
  return(obj)
}

#value getting
get_times<-function(obj)
{
  UseMethod("get_times",obj)
}

get_times.default<-function(obj)
{
  stop("Error in get_times: get_times not defined for this class")
}

get_times.tts<-function(obj)
{
  return(obj$times)
}

get_timescales<-function(obj)
{
  UseMethod("get_timescales",obj)
}

get_timescales.default<-function(obj)
{
  stop("Error in get_timescales: get_timescales not defined for this class")
}

get_timescales.tts<-function(obj)
{
  return(obj$timescales)
}

get_values<-function(obj)
{
  UseMethod("get_values",obj)
}

get_values.default<-function(obj)
{
  stop("Error in get_values: get_values not defined for this class")
}

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
