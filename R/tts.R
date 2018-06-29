#' Creator function for the \code{tts} class
#' 
#' The \code{tts} (time/timescale) class is for matrices for which the rows correspond 
#' to times and the columns correspond to timescales. This is a general class from 
#' which other classes inherit (e.g., \code{wt}, \code{wmf}, \code{wpmf}). \code{tts} 
#' inherits from the \code{list} class. 
#' 
#' @param times A numeric vector of increasing real values, spacing 1
#' @param timescales A numeric vector with positive entries
#' @param values A complex or numeric matrix of dimensions \code{length(times)} by 
#' \code{length(timescales)}
#' 
#' @return \code{tts} returns an object of class \code{tts}. Slots are: 
#' \item{times}{a numeric vector of evenly spaced times} 
#' \item{timescales}{a numeric vector of positive timescales}
#' \item{values}{a complex or numeric matrix of dimensions \code{length(times)} by \code{length(timescales)}}
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

tts<-function(times,timescales,values)
{
  errcheck_tts(times,timescales,values,"tts")
  res<-list(times=times,timescales=timescales,values=values)  
  class(res)<-c("tts","list")
  return(res)
}





