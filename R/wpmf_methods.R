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
  stop("Error in set_timescales: timescales should not be alterned for a wpmf object")
}

set_values.wpmf<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wpmf object")
}

#value getting - methods not needed, inherited from tts

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
