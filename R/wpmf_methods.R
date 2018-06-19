#Simple methods of the wpmf class

#value setting - these just throw an error, since we do not want
#individual components of a wpmf object changed as that breaks the
#consistency among the components
set_times.wpmf<-function(obj,newval)
{
  stop("Error in set_times: times scould not be altered for a wpmf object")
}

set_timescales.wpmf<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be altered for a wpmf object")
}

set_values.wpmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wpmf object")
}

set_dat<-function(obj,newval)
{
  UseMethod("set_dat",obj)
}

set_dat.default<-function(obj,newval)
{
  stop("Error in set_dat: set_dat not defined for this class")
}

set_dat.wpmf<-function(obj,newval)
{
  stop("Error in set_dat: dat should not be altered for a wpmf object")
}

set_signif<-function(obj,newval)
{
  UseMethod("set_signif",obj)  
}

set_signif.default<-function(obj,newval)
{
  stop("Error in set_signif: set_signif not defined for this class")
}

set_signif.wpmf<-function(obj,newval)
{
  stop("Error in set_signif: signif should not be altered for a wpmf object")
}

#value getting - methods not needed except for dat, others inherited from tts

get_dat<-function(obj)
{
  UseMethod("get_dat",obj)
}

get_dat.default<-function(obj)
{
  stop("Error in get_dat: get_dat not defined for this class")
}

get_dat.wpmf<-function(obj)
{
  return(obj$dat)
}

get_signif<-function(obj)
{
  UseMethod("get_signif",obj)
}

get_signif.default<-function(obj)
{
  stop("Error in get_signif: get_signif not defined for this class")
}

get_signif.wpmf<-function(obj)
{
  return(obj$signif)
}


#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#summary.wpmf<-function(obj)
#{
#  
#}

#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#print.wpmf<-function(obj)
#{
#  
#}

#What else? 
