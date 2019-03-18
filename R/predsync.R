#' Predicted synchrony of a wavelet linear model
#' 
#' Predicted synchrony of a \code{wlm} object. This is described in the
#' first paragraph of Appendix S15 of Sheppard et al (2019).
#' 
#' @param wlmobj A \code{wlm} object
#' 
#' @return \code{predsync} returns a \code{tts} object. Plotting the magnitude
#' (see \code{plotmag}) displays a picture of predicted synchrony versus time and
#' timescale that is comparable with the wavelet mean field (see \code{wmf}) of
#' the response variable of the model. Calling the \code{power} function on that 
#' \code{tts} object should give the same results as one of the columns of output
#' of \code{syncexpl}. Only \code{norm="powall"} implemented so far.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology 15, e1006744. doi: 10.1371/journal.pcbi.1006744
#' 
#' @seealso \code{\link{wlm}}, \code{\link{tts}}, \code{\link{plotmag}}, \code{\link{wmf}}, \code{\link{power}},
#' \code{\link{syncexpl}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-(-3:100)
#' ts1<-sin(2*pi*times/10)
#' ts2<-5*sin(2*pi*times/3)
#' artsig_x<-matrix(NA,11,length(times)) #the driver
#' for (counter in 1:11)
#' {
#'   artsig_x[counter,]<-ts1+ts2+rnorm(length(times),mean=0,sd=.5)
#' }
#' times<-0:100
#' artsig_y<-matrix(NA,11,length(times)) #the driven
#' for (counter1 in 1:11)
#' {
#'   for (counter2 in 1:101)
#'   {
#'     artsig_y[counter1,counter2]<-mean(artsig_x[counter1,counter2:(counter2+2)])
#'   }
#' }
#' artsig_y<-artsig_y+matrix(rnorm(length(times)*11,mean=0,sd=1),11,length(times))
#' artsig_x<-artsig_x[,4:104]
#' artsig_i<-matrix(rnorm(11*length(times)),11,length(times)) #the irrelevant
#' artsig_x<-cleandat(artsig_x,times,1)$cdat
#' artsig_y<-cleandat(artsig_y,times,1)$cdat
#' artsig_i<-cleandat(artsig_i,times,1)$cdat

#' dat<-list(driven=artsig_y,driver=artsig_x,irrelevant=artsig_i)
#' resp<-1
#' pred<-2:3
#' norm<-"powall"
#' wlmobj<-wlm(dat,times,resp,pred,norm)
#' 
#' res<-predsync(wlmobj)
#' 
#' @export

predsync<-function(wlmobj)
{
  UseMethod("predsync",wlmobj)
}

#' @export
predsync.default<-function(wlmobj)
{
  stop("Error in predsync: method not defined for this class")
}

#' @rdname predsync
#' @export
predsync.wlm<-function(wlmobj)
{
  #get the necessary slots
  modval<-get_modval(wlmobj)
  coher<-get_coher(wlmobj)
  times<-get_times(wlmobj)
  timescales<-get_timescales(wlmobj)
  norm<-get_norm(wlmobj)
  
  #only powall implemented
  if (norm!="powall")
  {
    stop("Error in predsync: this value of norm not implemented yet")
  }
  
  #get the model-predicted synchrony
  modval<-normforcoh(modval,norm)
  res<-apply(FUN=mean,X=modval,MARGIN=c(2,3)) #model mean field  
  for (tscounter in 1:length(timescales))
  { #multiply by model-response coherence
    res[,tscounter]<-res[,tscounter]*coher[tscounter]
  }
  
  #prepare result and return
  res<-tts(times,timescales,res)
  return(res)
}
