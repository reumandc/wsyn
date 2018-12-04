#' For plotting the phases of values in \code{tts} and \code{coh} objects 
#' 
#' For plotting the phases of values in \code{tts} objects (and derived classes) 
#' against time and timescale, and \code{coh} objects against timescale
#'
#' @param object A \code{coh} object. 
#' @param bandprows The rows of \code{object$bandp} for which to display p-value results in the plot
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no file and uses the default graphics device.
#'    
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @examples
#' #Not written yet but need some
#' 
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot lines text

plotphase<-function(object,...)
{
  UseMethod("plotphase",object)
}

# #' @rdname plotphase
# #' @export
# plotphase.tts<-function()
# {
#   
# }

# #' @rdname plotphase
# #' @export
# plotphase.wt<-function()
# {
#   
# }

# #' @rdname plotphase
# #' @export
# plotphase.wmf<-function()
# {
#   
# }

# #' @rdname plotphase
# #' @export
# plotphase.wpmf<-function()
# {
#   
# }

#' @rdname plotphase
#' @export
plotphase.coh<-function(object,bandprows="all",filename=NA,...)
{
  #extract the needed slots
  timescales<-get_timescales(object)
  coher<-get_coher(object)
  bandp<-get_bandp(object)
  
  #error catch
  if (bandprows!="all" && !any(is.na(bandp)))
  {
    if (!is.numeric(bandprows))
    {
      stop("Error in plotphase.coh: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1]))
    {
      stop("Error in plotphase.coh: bandprows must contain row numbers for bandp")
    }
  }
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  #if bandp is absent, just plot the lines, no p-values
  if (any(is.na(bandp)))
  { 
    plot(log(1/timescales),Arg(coher),type="p",xaxt="n",
         ylim=c(-pi,pi),xlab="Timescales",ylab="Phase")
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
    
    if (!is.na(filename))
    {
      grDevices::dev.off()
    }
    return(NULL) 
  }
  
  #from here on is if bandp is present

  #get a new range to leave space for the p-values
  rg<-c(-pi,pi)
  prc<-0.15
  drg<-diff(rg)
  rg[2]<-rg[2]+dim(bandp)[1]*prc*drg
  
  #make the main plot
  x<-log(1/timescales)
  plot(x,Arg(coher),type="p",xaxt="n",yaxt="n",
       ylim=rg,xlab="Timescales",ylab="Phase",pch=20,cex=.6)
  lines(range(x),c(pi,pi),type='l')
  xlocs<-c(min(timescales),pretty(timescales,n=8))
  graphics::axis(side=1,at=log(1/xlocs),labels=xlocs)
  graphics::axis(side=2,at=c(-pi,-pi/2,0,pi/2,pi),labels=expression(-pi,-pi/2,0,pi/2,pi))

  #p-values
  if (bandprows!="all")
  {
    bandp<-bandp[bandprows,]
  }
  for (counter in 1:dim(bandp)[1])
  {
    b1<-unname(bandp[counter,1])
    b2<-unname(bandp[counter,2])
    mnphs<-unname(bandp[counter,4])
    htl<-rg[2]-(counter-1/4-.1)*prc*drg
    wwd<-.07*prc*drg
    lines(log(1/c(b1,b2)),c(htl,htl))
    lines(log(1/c(b1,b1)),c(htl-wwd,htl+wwd))
    lines(log(1/c(b2,b2)),c(htl-wwd,htl+wwd))
    htt<-rg[2]-(counter-1.2/2-.1)*prc*drg
    valh<-round(mnphs/pi,2)
    text(mean(log(1/c(b1,b2))),htt,
         bquote(bar(theta) == .(valh)),cex=0.66)
  }
  
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL) 
}

#' @rdname plotphase
#' @export
plotphase.default<-function(object,...)
{
  stop("Error in plotphase: method not defined for this clas")
}