#' For plotting the magnitude of values in \code{tts} and \code{coh} objects 
#' 
#' For plotting the magnitude of values in \code{tts} objects (and derived classes) 
#' against time and timescale, and \code{coh} objects against timescale
#'
#' @param object An object of class \code{tts} or some class that inherits from \code{tts} or of class \code{coh}
#' @param zlims z axis limits. If specified, must encompass the range of \code{Mod(get_values(object))}. Default NULL uses this range.
#' @param neat Logical. Should timescales with no values be trimmed?
#' @param colorfill Color spectrum to use, set through colorRampPalette. Default value NULL produces jet colors from Matlab.
#' @param sigthresh Significance threshold(s). Numeric vector with values between 0 and 1. Typically 0.95, 0.99, 0.999, etc. For \code{wpmf} objects, contours are plotted at these values; for \code{coh} objects the threshholds are plotted on coherence plots.
#' @param colorbar Logical. Should a colorbar legend be plotted?
#' @param title Title for the top of the plot.
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no file and uses the defauly graphics device.
#' @param bandprows The rows of \code{object$bandp} for which to display results in \code{coh} plots
#' @param ... Additional graphics parameters passed to \code{image} (\code{graphics} package) if \code{colorbar==FALSE}, or to \code{image.plot} (\code{fields} package) if \code{colorbar==TRUE}
#' 
#' @details For \code{coh} objects, object$coher is plotted using a solid red line, and 
#' object$signif$coher is plotted using a dashed red line. The two coherences agree except
#' for \code{sigmethod="fast"}, for which they are close. The dashed line is what should be
#' compared to the distribution of surrogate coherences (black lines, if \code{signif} is 
#' not \code{NA}). Horizontal axis ticks are labelled as timescales, but are spaced on the 
#' axis as log(1/timescale), i.e., log frequencies.
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
#' @importFrom fields image.plot
#' @importFrom graphics image axis par
#' @importFrom grDevices colorRampPalette pdf dev.off
#' @importFrom stats quantile

plotmag<-function(object,...)
{
  UseMethod("plotmag",object)
}

#' @rdname plotmag
#' @export
plotmag.tts<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,colorbar=TRUE,title=NULL,filename=NA,...)
{
  wav<-Mod(get_values(object))
  times<-get_times(object)
  timescales<-get_timescales(object)
  
  if(is.null(zlims)){
    zlims<-range(wav,na.rm=T)
  }else
  {
    rg<-range(wav,na.rm=T)
    if (rg[1]<zlims[1] || rg[2]>zlims[2])
    {
      stop("Error in plotmag.tts: zlims must encompass the z axis range of what is being plotted")
    }
  }
  if(neat){
    inds<-which(!is.na(colMeans(wav,na.rm=T)))
    wav<-wav[,inds]
    timescales<-timescales[inds]
  }
  if(is.null(colorfill)){
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill<-grDevices::colorRampPalette(jetcolors)
  }
  ylocs<-pretty(timescales,n=8)
  xlocs<-pretty(times,n=8)
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  if (!colorbar)
  {
    graphics::image(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
          ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }else
  {
    fields::image.plot(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
          ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL)
}

#' @rdname plotmag
#' @export
#plotmag.wt just the same as tts, we define it explicitly instead of inheriting for the sake of the help files
plotmag.wt<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,colorbar=TRUE,title=NULL,filename=NA,...)
{
  return(plotmag.tts(object,zlims,neat,colorfill,colorbar,title,filename,...))
}

#' @rdname plotmag
#' @export
#plotmag.wmf just the same as tts, we define it explicitly instead of inheriting for the sake of the help files
plotmag.wmf<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,colorbar=TRUE,title=NULL,filename=NA,...)
{
  return(plotmag.tts(object,zlims,neat,colorfill,colorbar,title,filename,...))
}

#' @rdname plotmag
#' @export
plotmag.wpmf<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,sigthresh=0.95,colorbar=TRUE,title=NULL,filename=NA,...)
{
  wav<-Mod(get_values(object))
  times<-get_times(object)
  timescales<-get_timescales(object)
  signif<-get_signif(object)
  
  if (any(sigthresh>=1 | sigthresh<=0))
  {
    stop("Error in plotmag.wpmf: inappropriate value for sigthresh")
  }
  if(is.null(zlims)){
    zlims<-range(wav,na.rm=T)
  }else
  {
    rg<-range(wav,na.rm=T)
    if (rg[1]<zlims[1] || rg[2]>zlims[2])
    {
      stop("Error in plotmag.wpmf: zlims must encompass the z axis range of what is being plotted")
    }
  }
  if(neat){
    inds<-which(!is.na(colMeans(wav,na.rm=T)))
    wav<-wav[,inds]
    timescales<-timescales[inds]
    if (!is.na(signif) && (signif[[1]] %in% c("fft","aaft")))
    {
      signif[[3]]<-signif[[3]][,inds]
    }
  }
  if(is.null(colorfill)){
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill<-grDevices::colorRampPalette(jetcolors)
  }
  ylocs<-pretty(timescales,n=8)
  xlocs<-pretty(times,n=8)
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  if (!colorbar)
  {
    graphics::image(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
          ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }else
  {
    fields::image.plot(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
               ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }
  if (!all(is.na(signif)))
  {
    graphics::par(new=T)
    if (signif[[1]]=="quick")
    {
      q<-stats::quantile(signif[[2]],sigthresh)
      graphics::contour(x=times,y=log2(timescales),z=wav,levels=q,drawlabels=F,lwd=2,
              xaxs="i",xaxt="n",yaxt="n",xaxp=c(0,1,5),las = 1,frame=F)
    }
    if (signif[[1]] %in% c("fft","aaft"))
    {
      
      graphics::contour(x=times,y=log2(timescales),z=signif[[3]],levels=sigthresh,
              drawlabels=F,lwd=2,xaxs="i",xaxt="n",yaxt="n",xaxp=c(0,1,5),
              las = 1,frame=F)
    }
  }
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL) 
}

#' @rdname plotmag
#' @export
plotmag.coh<-function(object,sigthresh=c(0.95,.99),bandprows="all",filename=NA)
{
  #extract the needed slots
  timescales<-get_timescales(object)
  coher<-get_coher(object)
  signif<-get_signif(object)
  bandp<-get_bandp(object)
  
  #error catch
  if (any(sigthresh>=1 | sigthresh<=0))
  {
    stop("Error in plotmag.coh: inappropriate value for sigthresh")
  }
  if (bandprows!="all" && !any(is.na(bandp)))
  {
    if (!is.numeric(bandprows))
    {
      stop("Error in plotmag.coh: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1]))
    {
      stop("Error in plotmag.coh: bandprows must contain row numbers for bandp")
    }
  }
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  #if signif is absent, then just plot coher v timescales
  if (any(is.na(signif)))
  { 
    plot(log(1/timescales),Mod(coher),type="l",lty="solid",xaxt="n",col="red",
         xlab="Timescales",ylab="|Coherence|")
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
    
    if (!is.na(filename))
    {
      grDevices::dev.off()
    }
    return(NULL)
  } 
    
  #from here on is if signif is present
  
  #get quantiles for surrogate coherences
  qs<-apply(X=Mod(signif$scoher),FUN=stats::quantile,MARGIN=2,prob=sigthresh)
  if (length(sigthresh)==1){qs<-matrix(qs,1,length(qs))}
  
  #if bandp is absent, just plot the lines, no p-values
  if (any(is.na(bandp)))
  { 
    rg<-range(Mod(coher),Mod(signif$coher),qs,na.rm=T) 
    plot(log(1/timescales),Mod(coher),type="l",lty="solid",xaxt="n",col="red",
         ylim=rg,xlab="Timescales",ylab="|Coherence|")
    xlocs<-c(min(timescales),pretty(timescales,n=8))
    graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
    lines(log(1/timescales),Mod(signif$coher),type="l",lty="dashed",col="red")
    for (counter in 1:dim(qs)[1])
    {
      lines(log(1/timescales),qs[counter,])
    }
    
    if (!is.na(filename))
    {
      grDevices::dev.off()
    }
    return(NULL) 
  } 
  
  #from here on is if signif and bandp are both present
  
  rg<-range(Mod(coher),Mod(signif$coher),qs,na.rm=T)
  prc<-0.15
  drg<-diff(rg)
  rg[2]<-rg[2]+dim(bandp)[1]*prc*drg
  plot(log(1/timescales),Mod(coher),type="l",lty="solid",xaxt="n",col="red",
       ylim=rg,xlab="Timescales",ylab="|Coherence|")
  xlocs<-c(min(timescales),pretty(timescales,n=8))
  graphics::axis(side=1,at=log(1/xlocs),labels=xlocs) 
  lines(log(1/timescales),Mod(signif$coher),type="l",lty="dashed",col="red")
  for (counter in 1:dim(qs)[1])
  {
    lines(log(1/timescales),qs[counter,])
  }
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
  
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
  return(NULL)
}

#' @rdname plotmag
#' @export
plotmag.default<-function(object,...)
{
  stop("Error in plotmag: method not defined for this class")
}