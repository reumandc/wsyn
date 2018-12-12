#' Power of a \code{tts} object
#' 
#' Returns the power of a \code{tts} object, i.e., the mean over
#' time of the squared magnitude (which is a function of timescale)
#' 
#' @param object A \code{tts} object
#' 
#' @return \code{power} returns a data frame with columns timescales and power
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{tts}}, \code{\link{wt}}, \code{\link{wmf}}, \code{\link{wpmf}},
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-1:10
#' timescales<-1:10
#' values<-matrix(rep(complex(modulus=1,argument=2*pi*c(0:9)/10),times=10),10,10)
#' ttsobj<-tts(times,timescales,values)
#' res<-power(ttsobj)
#'  
#' @export

power<-function(object)
{
  UseMethod("power",object)
}

#' @export
power.default<-function(object)
{
  stop("Error in power: method not defined for this class")
}

#' @rdname power
#' @export
power.tts<-function(object)
{
  #extract the relevant components
  times<-get_times(object)
  timescales<-get_timescales(object)
  values<-get_values(object)
  
  #get the power and set up the result data frame
  pow<-apply(FUN=mean,X=(Mod(values))^2,MARGIN=2,na.rm=T)
  res<-data.frame(timescales=timescales,power=pow)
  
  return(res)
}