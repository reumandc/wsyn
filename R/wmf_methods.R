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

#value getting - methods not needed except for dat, others inherited from tts

#' @export
get_dat.wmf<-function(obj)
{
  return(obj$dat)
}

#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#summary.wmf<-function(obj)
#{
#  
#}

#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#print.wmf<-function(obj)
#{
#  
#}

#What else? 
