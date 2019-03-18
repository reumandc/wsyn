#' For plotting the phases of values in \code{tts} and \code{coh} objects 
#' 
#' For plotting the phases of values in \code{tts} objects (and derived classes) 
#' against time and timescale, and \code{coh} objects against timescale
#'
#' @param object A \code{coh} object. 
#' @param bandprows The rows of \code{object$bandp} for which to display p-value results in the plot
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no file 
#' and uses the default graphics device.
#' @param ... Passed from the generic to specific methods. The plotphase.tss method passes it to 
#' fields::image.plot.
#'    
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; 
#' Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @seealso \code{\link{tts}}, \code{\link{wt}}, \code{\link{wmf}}, \code{\link{wpmf}}, \code{\link{coh}}, 
#' \code{\link{plotmag}}, \code{\link{plotrank}}, \code{browseVignettes("wsyn")}
#' 
#' @examples
#' #For a tts object
#' times<-1:100
#' timescales<-1:100
#' cplx<-complex(modulus=1,argument=seq(from=-pi,to=pi,length.out=100))
#' values1<-matrix(cplx,length(times),length(timescales))
#' tts1<-tts(times,timescales,values1)  
#' plotphase(tts1)
#' 
#' #For a coh oject
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
#' res<-coh(dat1=artsig_x,dat2=artsig_y,times=times,norm="powall",sigmethod="fast",nrand=50,
#'          f0=0.5,scale.max.input=28)
#' res<-bandtest(res,c(2,4))
#' res<-bandtest(res,c(4,30))
#' res<-bandtest(res,c(8,12))
#' plotphase(res)
#' 
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot lines text axis

plotphase<-function(object,...)
{
  UseMethod("plotphase",object)
}

#' @rdname plotphase
#' @export
plotphase.tts<-function(object,filename=NA,...)
{
  zval<-Arg(get_values(object))
  times<-get_times(object)
  timescales<-get_timescales(object)
  
  ylocs<-pretty(timescales,n=8)
  xlocs<-pretty(times,n=8)
  
  colorfill<-grDevices::colorRampPalette(c("black","blue","white","red","black"))
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  fields::image.plot(x=times,y=log2(timescales),z=zval,xlab="Time",zlim=c(-pi,pi),
                     ylab="Timescale",axes=F,col=colorfill(100),...)
  graphics::axis(1,at = xlocs,labels=xlocs)
  graphics::axis(2,at = log2(ylocs),labels = ylocs)
  
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
}

#' @rdname plotphase
#' @export
#plotphase.wt just the same as tts, we define it explicitly instead of inheriting for the sake of the help files
plotphase.wt<-function(object,filename=NA,...)
{
  return(plotphase.tts(object,filename=NA,...))
}

#' @rdname plotphase
#' @export
#plotphase.wmf just the same as tts, we define it explicitly instead of inheriting for the sake of the help files
plotphase.wmf<-function(object,filename=NA,...)
{
  return(plotphase.tts(object,filename=NA,...))
}

#' @rdname plotphase
#' @export
#plotphase.wpmf just the same as tts, we define it explicitly instead of inheriting for the sake of the help files
plotphase.wpmf<-function(object,filename=NA,...)
{
  return(plotphase.tts(object,filename=NA,...))
}

#' @rdname plotphase
#' @export
plotphase.coh<-function(object,bandprows="all",filename=NA,...)
{
  #extract the needed slots
  timescales<-get_timescales(object)
  coher<-get_coher(object)
  bandp<-get_bandp(object)
  
  #error catch
  if (!identical(bandprows,"all") && !any(is.na(bandp)))
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
    if (b1<min(timescales)){b1<-min(timescales)}
    b2<-unname(bandp[counter,2])
    if (b2>max(timescales)){b2<-max(timescales)}
    mnphs<-unname(bandp[counter,4])
    htl<-rg[2]-(counter-1/4-.1)*prc*drg
    wwd<-.07*prc*drg
    lines(log(1/c(b1,b2)),c(htl,htl))
    lines(log(1/c(b1,b1)),c(htl-wwd,htl+wwd))
    lines(log(1/c(b2,b2)),c(htl-wwd,htl+wwd))
    htt<-rg[2]-(counter-1.2/2-.1)*prc*drg
    valh<-round(mnphs,2)
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
  stop("Error in plotphase: method not defined for this class")
}