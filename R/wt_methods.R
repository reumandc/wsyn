#Simple methods of the wt class

#value setting - these just throw an error, since we do not want
#individual components of a wt object changed as that breaks the
#consistency among the components

#' @export
set_times.wt<-function(obj,newval)
{
  stop("Error in set_times: times scould not be altered for a wt object")
}

#' @export
set_timescales.wt<-function(obj,newval)
{
  stop("Error in set_timescales: timescales should not be alterned for a wt object")
}

#' @export
set_values.wt<-function(obj,newval)
{
  stop("Error in set_values: values should not be altered for a wt object")
}

#' @export
set_dat.wt<-function(obj,newval)
{
  stop("Error in set_dat: values should not be altered for a wt object")
}

#value getting - methods not needed except for dat, others inherited from tts

#' @export
get_dat.wt<-function(obj)
{
  return(obj$dat)
}

#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#summary.wt<-function(obj)
#{
#  
#}

#This is based on an existing generic so not sure if this will work like this.
#Also, might not be necessary, perhaps the tts one is enough.
#print.wt<-function(obj)
#{
#  
#}

#What else? 
