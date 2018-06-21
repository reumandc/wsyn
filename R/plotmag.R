#' For plotting the magnitude of values in \code{tts} objects 
#' 
#' For plotting the magnitude of values in \code{tts} objects (and derived classes) 
#' against time and timescale
#'
#' @param object An object of class \code{tts} or some class that inherits from \code{tts}
#' @param zlims z axis limits. If specified, must encompass the range of \code{Mod(get_values(object))}. Default NULL uses this range.
#' @param neat Logical. Should timescales with no values be trimmed?
#' @param colorfill Color spectrum to use, set through colorRampPalette. Default value NULL produces jet colors from Matlab.
#' @param sigthresh Significance threshold(s) for \code{wpmf} objects. Numeric vector with values between 0 and 1 (typically 0.95, 0.99, 0.999, etc.). Contours are plotted at these values.
#' @param colorbar Logical. Should a colorbar legend be plotted?
#' @param title Title for the top of the plot.
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no file and uses the defauly graphics device.
#' @param ... Additional graphics parameters passed to \code{image} (\code{graphics} package) if \code{colorbar==FALSE}, or to \code{image.plot} (\code{fields} package) if \code{colorbar==TRUE}
#' 
plotmag<-function(object,...)
{
  UseMethod("plotmag",object)
}

#' @rdname
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
    colorfill<-colorRampPalette(jetcolors)
  }
  ylocs<-pretty(timescales,n=8)
  xlocs<-pretty(times,n=8)
  
  if (!is.na(filename))
  {
    pdf(paste0(filename,".pdf"))
  }
  if (!colorbar)
  {
    image(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
          ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    axis(1,at = xlocs,labels=xlocs)
    axis(2,at = log2(ylocs),labels = ylocs)
  }else
  {
    image.plot(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
          ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    axis(1,at = xlocs,labels=xlocs)
    axis(2,at = log2(ylocs),labels = ylocs)
  }
  if (!is.na(filename))
  {
    dev.off()
  }
  return(NULL)
}

#plotmag.wt not necessary - inherits from tts

#plotmag.wmf not necessary - inherits from tts

#' @rdname 
#' @export
plotmag.wpmf<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,sigthresh=0.95,colorbar=TRUE,title=NULL,filename=NA,...)
{
 
}

#' @rdname 
#' @export
plotmag.default<-function(object,...)
{
  stop("Error in plotmag: method not defined for this class")
}