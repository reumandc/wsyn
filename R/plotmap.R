#' Map clusters from a \code{clust} object
#' 
#' Produces a map of the locations of sampling for a \code{clust} object, with colors indicating 
#' module (cluster) identity.
#' The sizes of nodes (locations) are scaled according to the strength of membership in its module.
#' 
#' @param inclust A \code{clust} object, as created with \code{wsyn::clust}
#' @param spltlvl The split level in the clustering to use. This is the index of inclust$clusters.
#' Default the final split.
#' @param nodesize A length = 2 vector giving the minimum and maximum node size for plotting. Defaults to c(1,3).
#' @param filename a filename, possibly including path info, but without a file extension. If present,
#' exports the plot as a .pdf using the specified filename. Default \code{NA} uses the default plotting
#' device.
#' 
#' @return \code{plotmap} produces a map.
#'
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#'
#' @references Walter, J. A., et al. (2017) The geography of spatial synchrony. Ecology Letters. 
#' doi: 10.1111/ele.12782
#'
#' @seealso \code{\link{clust}}, \code{browseVignettes("wsyn")}
#'
#' @examples
#' Tmax<-500
#' tim<-1:Tmax
#' ts1<-sin(2*pi*tim/5)
#' ts1s<-sin(2*pi*tim/5+pi/2)
#' ts2<-sin(2*pi*tim/12)
#' ts2s<-sin(2*pi*tim/12+pi/2)
#' gp1A<-1:2
#' gp1B<-3:4
#' gp2A<-5:6
#' gp2B<-7:8
#' d<-matrix(NA,Tmax,8)
#' d[,c(gp1A,gp1B)]<-ts1
#' d[,c(gp2A,gp2B)]<-ts1s
#' d[,c(gp1A,gp2A)]<-d[,c(gp1A,gp2A)]+matrix(ts2,Tmax,4)
#' d[,c(gp1B,gp2B)]<-d[,c(gp1B,gp2B)]+matrix(ts2s,Tmax,4)
#' d<-d+matrix(rnorm(Tmax*8,0,2),Tmax,8)
#' d<-t(d)
#' d<-cleandat(d,1:Tmax,1)$cdat
#' coords<-data.frame(X=c(rep(1,4),rep(2,4)),Y=rep(c(1:2,4:5),times=2))
#' cl5<-clust(dat=d,times=1:Tmax,coords=coords,method="ReXWT",tsrange=c(4,6))
#' plotmap(cl5)
#' cl12<-clust(dat=d,times=1:Tmax,coords=coords,method="ReXWT",tsrange=c(11,13))
#' plotmap(cl12)
#'
#' @export
#' @importFrom graphics legend plot 
#' @importFrom grDevices colorRampPalette pdf dev.off

plotmap<-function(inclust, spltlvl=length(inclust$clusters), nodesize=c(1,3), filename=NA)
{
  #some checking of validity of inputs
  if(!inherits(inclust,"clust"))
  {
    stop("Error in plotmap: inclust must be a clust object")
  }
  if(max(inclust$clusters[[spltlvl]])>9){
    stop("Error in plotmap: more than 9 modules, plotmap cannot proceed")
  }
  
  #convert inclust$coords to common format
  if(all(c("X","Y") %in% names(inclust$coords)))
  {
    coords<-data.frame(X=inclust$coords$X,Y=inclust$coords$Y)
    x_label<-"X"
    y_label<-"Y"
  }
  if(all(c("lat","lon") %in% names(inclust$coords)))
  {
    coords<-data.frame(X=inclust$coords$lon,Y=inclust$coords$lat)
    x_label<-"longitude"
    y_label<-"latitude"
  }
  if(all(c("latitude","longitude") %in% names(inclust$coords)))
  {
    coords<-data.frame(X=inclust$coords$longitude,Y=inclust$coords$latitude)
    x_label<-"longitude"
    y_label<-"latitude"
  }
  
  #make color ramp --  this is Set1 from RColorBrewer
  pal=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
  
  if(!is.na(filename)){
    pdf(paste0(filename,".pdf"))
  }
  
  #expand right side margin for the legend
  par.mar<-par("mar")
  mar.new<-par.mar
  mar.new[4]<-6.1
  par(mar=mar.new,xpd=T)

  membwgt<-inclust$modres[[spltlvl]]$nodeQ-min(inclust$modres[[spltlvl]]$nodeQ)
  membwgt<-membwgt/max(membwgt)
  nodecex<-membwgt*(nodesize[2]-nodesize[1]) + nodesize[1]

  plot(coords[,1], coords[,2], pch=16, cex=nodecex, col=pal[unlist(inclust$clusters[spltlvl])],
       xlab=x_label,ylab=y_label)

  #add legends
  legx<-par('usr')[2] + 0.01*abs(diff(par('usr')[1:2]))
  legy1<-par('usr')[4]
  leg1<-legend(legx,legy1,legend=paste0("module ",1:max(unlist(inclust$clusters[spltlvl]))), pch=16, col=
           pal[1:max(unlist(inclust$clusters[spltlvl]))],title="Membership",bty="n")
  legy2<-legy1 - leg1$rect$h
  labs<-round(c(min(inclust$modres[[spltlvl]]$nodeQ,na.rm=T),
               mean(inclust$modres[[spltlvl]]$nodeQ,na.rm=T),
               max(inclust$modres[[spltlvl]]$nodeQ,na.rm=T)),digits=3)
  sizes<-c(min(nodecex),mean(nodecex),max(nodecex))
  leg2<-legend(legx,legy2,legend=labs,pt.cex=sizes,pch=1,title="Node weight",bty="n")

  par(mar=par.mar) #reset 'mar' graphics parameter
  
  if(!is.na(filename)){dev.off()}
}