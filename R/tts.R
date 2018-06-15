#' Creator function for the \code{tts} class
#' 
#' The \code{tts} (time/timescale) class is for matrices for which the rows correspond 
#' to times and the columns correspond to timescales. This is a general class from 
#' which other classes inherit (e.g., \code{wt}, \code{wmf}, \code{wpmf}). \code{tts} 
#' inherits from the \code{list} class. 
#' 
#' @param times A numeric vector of evenly spaced increasing real values
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
  
  #2) timescale should be a numeric vector, think about what else it should be, e.g., positive, decreasing/increasing, what?
  if (!is.numeric(timescales))
  {
    stop("Error in tts class: timescales must be numeric")
  }
  if (any(timescales<=0))
  {
    stop("Error in tts class: timescales must be positive")
  }
  if ()
    #3) values a numeric or complex-valued matrix, length(times) by length(timescales)
    if (!is.numeric(values) && !is.complex(values))
    {
      stop("Error in tts class: values must be numeric or complex")
    }
  if (!is.matrix(values))
  {
    stop("Error in tts class: values must be a matrix")
  }
  if (dim(values)[1]!=length(times) || dim(values)[2]!=length(timescales))
  {
    stop("Error in tts class: dimensions of values should agree with lengths of times and timescales")
  }
  
  res<-list(times=times,timescales=timescales,values=values)  
  class(res)<-c("tts","list")
  return(res)
}





