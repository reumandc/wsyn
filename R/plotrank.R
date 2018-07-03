#' Plots \code{ranks} slot for \code{coh} objects
#' 
#' @exportMethod 
#' @importFrom graphics axis
#' @importFrom grDevices pdf dev.off

plotrank<-function(object,...)
{
  UseMethod("plotrank",object)
}

#' @rdname plotrank
#' @export
plotrank.coh<-function(object,sigthresh=0.95,bandprows="all",filename=NA)
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
  if (bandprows!="all" && !any(is.na(bandp)))
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
    if (bandprows!="all")
    {
      bandp<-bandp[bandprows,]
    }
    for (counter in 1:dim(bandp)[1])
    {
      b1<-unname(bandp[counter,1])
      b2<-unname(bandp[counter,2])
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
