#' Plots \code{ranks} slot for \code{coh} and \code{wlmtest} objects
#' 
#' Plots the \code{ranks} slot for \code{coh} and \code{wlmtest} objects to help identify statistical 
#' significance of coherence
#' 
#' @param object A \code{coh} or \code{wlmtest} object. Must have a non-\code{NA} \code{signif} slot.
#' @param sigthresh Significance threshold(s). Numeric vector with values between 0 and 1. Typically 
#' 0.95, 0.99, 0.999, etc. The threshhold(s) are plotted on the rank plot as dashed horizontal line(s).
#' @param bandprows The rows of \code{object$bandp} for which to display p-value results in the plot
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no file and 
#' uses the default graphics device.
#' @param ... Passed from the generic to specific methods. Not currently used.
#' 
#' @details The plot shows the modulus of \code{object$ranks$coher} versus \code{log(1/object$timescales)}.
#' Horizontal axis ticks are labeled as timescales, but are spaced on the axis as 
#' log(1/timescale), i.e., log frequencies. p-values from \code{object$bandp} are displayed
#' above the rank plot.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology 15, e1006744. doi: 10.1371/journal.pcbi.1006744
#' 
#' @seealso \code{\link{coh}}, \code{\link{wlmtest}}, \code{\link{bandtest}}, \code{\link{plotphase}},
#' \code{\link{plotmag}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' #For a coh object
#' times<-(-3:100)
#' ts1<-sin(2*pi*times/10)
#' ts2<-5*sin(2*pi*times/3)
#' artsig_x<-matrix(NA,11,length(times)) #the driver
#' for (counter in 1:11)
#' {
#'   artsig_x[counter,]=ts1+ts2+rnorm(length(times),mean=0,sd=1.5)
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
#' artsig_y<-artsig_y+matrix(rnorm(length(times)*11,mean=0,sd=3),11,length(times))
#' artsig_x<-artsig_x[,4:104]
#' artsig_x<-cleandat(artsig_x,times,1)$cdat
#' artsig_y<-cleandat(artsig_y,times,1)$cdat
#' res<-coh(dat1=artsig_x,dat2=artsig_y,times=times,norm="powall",sigmethod="fast",
#' nrand=100,f0=0.5,scale.max.input=28)
#' #use larger nrand for a real application
#' res<-bandtest(res,c(2,4))
#' res<-bandtest(res,c(8,12))
#' plotrank(res)
#'   
#' #For a wlmtest object, see vignette
#' 
#' @export
#' @importFrom graphics axis plot lines text
#' @importFrom grDevices pdf dev.off

plotrank<-function(object,...)
{
  UseMethod("plotrank",object)
}

#' @rdname plotrank
#' @export
plotrank.coh<-function(object,sigthresh=0.95,bandprows="all",filename=NA,...)
{
  #error check
  if (any(is.na(object$signif)))
  {
    stop("Error in plotrank.coh: plotrank.coh needs a signif slot")
  }
  
  #extract the needed slots
  ranks<-get_ranks(object)
  if (any(is.na(ranks)))
  {
    object<-addranks(object)
    ranks<-get_ranks(object)
  }
  bandp<-get_bandp(object)
  timescales<-get_timescales(object)
  
  #more error check
  if (any(sigthresh>=1 | sigthresh<=0))
  {
    stop("Error in plotrank.coh: inappropriate value for sigthresh")
  }
  if (!identical(bandprows,"all") && !any(is.na(bandp)))
  {
    if (!is.numeric(bandprows))
    {
      stop("Error in plotrank.coh: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1]))
    {
      stop("Error in plotrank.coh: bandprows must contain row numbers for bandp")
    }
  }

  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  if (any(is.na(bandp)))
  { #if bandp is absent, just plot the lines, no p-values
    x<-log(1/timescales)
    plot(x,ranks$coher,type="l",lty="solid",xaxt="n",col="red",
         xlab="Timescales",ylab="Fract surr gt",ylim=c(0.5,1))
    for (counter in 1:length(sigthresh))
    {
      lines(range(x),c(sigthresh[counter],sigthresh[counter]),lty='dashed')
    }
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
  }else
  {  #if bandp is present, plot p-values, too
    #get the new vertical axis range to fit the p-vals
    rg<-c(0.5,1)
    prc<-0.15
    drg<-diff(rg)
    rg[2]<-rg[2]+dim(bandp)[1]*prc*drg
    
    #plot
    x<-log(1/timescales)
    plot(x,ranks$coher,type="l",lty="solid",xaxt="n",col="red",
         xlab="Timescales",ylab="Fract surr gt",ylim=rg)
    lines(range(x),c(1,1),type='l',lty="solid")
    for (counter in 1:length(sigthresh))
    {
      lines(range(x),c(sigthresh[counter],sigthresh[counter]),lty='dashed')
    }
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
    
    #add the p-vals
    if (!identical(bandprows,"all"))
    {
      bandp<-bandp[bandprows,]
    }
    for (counter in 1:dim(bandp)[1])
    {
      b1<-unname(bandp[counter,1])
      if (b1<min(timescales)){b1<-min(timescales)}
      b2<-unname(bandp[counter,2])
      if (b2>max(timescales)){b2<-max(timescales)}
      p<-unname(bandp[counter,3])
      htl<-rg[2]-(counter-1/4-.1)*prc*drg
      wwd<-.07*prc*drg
      lines(log(1/c(b1,b2)),c(htl,htl))
      lines(log(1/c(b1,b1)),c(htl-wwd,htl+wwd))
      lines(log(1/c(b2,b2)),c(htl-wwd,htl+wwd))
      htt<-rg[2]-(counter-1.2/2-.1)*prc*drg
      text(mean(log(1/c(b1,b2))),htt,paste0("p=",round(p,4)),cex=0.66)
    }
  }
  
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL)
}

#' @rdname plotrank
#' @export
plotrank.wlmtest<-function(object,sigthresh=0.95,bandprows="all",filename=NA,...)
{
  #extract the needed slots
  ranks<-get_ranks(object)
  if (any(is.na(ranks)))
  {
    object<-addranks(object)
    ranks<-get_ranks(object)
  }
  bandp<-get_bandp(object)
  timescales<-get_timescales(get_wlmobj(object))
  
  #more error check
  if (any(sigthresh>=1 | sigthresh<=0))
  {
    stop("Error in plotrank.wlmtest: inappropriate value for sigthresh")
  }
  if (!identical(bandprows,"all") && !any(is.na(bandp)))
  {
    if (!is.numeric(bandprows))
    {
      stop("Error in plotrank.wlmtest: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1]))
    {
      stop("Error in plotrank.wlmtest: bandprows must contain row numbers for bandp")
    }
  }
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  if (any(is.na(bandp)))
  { #if bandp is absent, just plot the lines, no p-values
    x<-log(1/timescales)
    plot(x,ranks$coher,type="l",lty="solid",xaxt="n",col="red",
         xlab="Timescales",ylab="Fract surr gt",ylim=c(0.5,1))
    for (counter in 1:length(sigthresh))
    {
      lines(range(x),c(sigthresh[counter],sigthresh[counter]),lty='dashed')
    }
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
  }else
  {  #if bandp is present, plot p-values, too
    #get the new vertical axis range to fit the p-vals
    rg<-c(0.5,1)
    prc<-0.15
    drg<-diff(rg)
    rg[2]<-rg[2]+dim(bandp)[1]*prc*drg
    
    #plot
    x<-log(1/timescales)
    plot(x,ranks$coher,type="l",lty="solid",xaxt="n",col="red",
         xlab="Timescales",ylab="Fract surr gt",ylim=rg)
    lines(range(x),c(1,1),type='l',lty="solid")
    for (counter in 1:length(sigthresh))
    {
      lines(range(x),c(sigthresh[counter],sigthresh[counter]),lty='dashed')
    }
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
    
    #add the p-vals
    if (!identical(bandprows,"all"))
    {
      bandp<-bandp[bandprows,]
    }
    for (counter in 1:dim(bandp)[1])
    {
      b1<-unname(bandp[counter,1])
      if (b1<min(timescales)){b1<-min(timescales)}
      b2<-unname(bandp[counter,2])
      if (b2>max(timescales)){b2<-max(timescales)}
      p<-unname(bandp[counter,3])
      htl<-rg[2]-(counter-1/4-.1)*prc*drg
      wwd<-.07*prc*drg
      lines(log(1/c(b1,b2)),c(htl,htl))
      lines(log(1/c(b1,b1)),c(htl-wwd,htl+wwd))
      lines(log(1/c(b2,b2)),c(htl-wwd,htl+wwd))
      htt<-rg[2]-(counter-1.2/2-.1)*prc*drg
      text(mean(log(1/c(b1,b2))),htt,paste0("p=",round(p,4)),cex=0.66)
    }
  }
  
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL)
}

#' @rdname plotrank
#' @export
plotrank.default<-function(object,...)
{
  stop("Error in plotrank: method not defined for this class")
}
