#' Amount of synchrony explained, and related quantities
#' 
#' Gives amount of synchrony explained by a wavelet linear model, as a function of 
#' timescale, and related quantities (see details)
#' 
#' @param object A \code{wlm} object
#' 
#' @return \code{syncexpl} returns a data frame with columns for \code{timescales},
#' \code{sync} (the time-averaged square magnitude of the wavelet mean field of the
#' response transforms), \code{syncexpl} (synchrony explained by the model 
#' predictors), columns named for each predictor (synchrony explained by that 
#' predictor), \code{interactions} (synchrony explained by all interaction effects),
#' columns named for each pair of predictors (synchrony explained by individual
#' pairwise interactions). There are also columns for \code{crossterms} and 
#' \code{resids} (residuals). The cross terms must be small for a given timescale band for
#' the other results to be meaningful. All columns are functions of timescales.
#' 
#' @details This function only works for \code{norm="powall"} at present. See
#' Sheppard et al (2018) for details of the meaning and computation of the 
#' columns.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology. In press.
#' 
#' @seealso \code{\link{wlm}}, \code{\link{predsync}}, \code{\link{wlmtest}},
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
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
#' artsig_i<-matrix(rnorm(11*length(times)),11,length(times)) #the irrelevant
#' artsig_x<-cleandat(artsig_x,times,1)$cdat
#' artsig_y<-cleandat(artsig_y,times,1)$cdat
#' artsig_i<-cleandat(artsig_i,times,1)$cdat
#' 
#' dat<-list(driven=artsig_y,driver=artsig_x,irrelevant=artsig_i)
#' resp<-1
#' pred<-2:3
#' norm<-"powall"
#' wlmobj<-wlm(dat,times,resp,pred,norm)
#' 
#' res<-syncexpl(wlmobj)
#'  
#' @export

syncexpl<-function(object)
{
  UseMethod("syncexpl",object)
}

#' @export
syncexpl.default<-function(object)
{
  stop("Error in syncexpl: method not defined for this class")
}

#' @rdname syncexpl
#' @export
syncexpl.wlm<-function(object)
{
  #get the necessary slots
  modval<-get_modval(object)
  coher<-get_coher(object)
  timescales<-get_timescales(object)
  norm<-get_norm(object)
  wts<-object$wts
  dat<-object$dat
  
  #only powall implemented so far
  if (norm!="powall")
  {
    stop("Error in syncexpl: this value of norm not implemented yet")
  }
  
  #receptacle for results
  res<-data.frame(timescales=timescales,
                  sync=NA*numeric(length(timescales)),
                  syncexpl=NA*numeric(length(timescales)),
                  crossterms=NA*numeric(length(timescales)),
                  resids=NA*numeric(length(timescales)))
  if (is.null(names(dat)))
  {
    pnames<-paste0("pred",1:(length(dat)-1))
  }else
  {
    pnames<-names(dat)
    pnames<-pnames[-1]
  }
  for (counter in 1:length(pnames))
  {
    res[,pnames[counter]]<-NA*numeric(1)
  }
  if (length(pnames)>1)
  {
    res[,"interactions"]<-NA*numeric(1)
    for (c1 in 1:(length(pnames)-1))
    {
      for (c2 in (c1+1):length(pnames))
      {
        res[,paste0(pnames[c1],"_",pnames[c2])]<-NA*numeric(1)
      }
    }
  }
  
  #compute the actual synchrony of the response
  rmf<-apply(FUN=mean,X=wts[[1]],MARGIN=c(2,3)) #response variable mean field
  res$sync<-apply(FUN=mean,X=(Mod(rmf))^2,MARGIN=2,na.rm=TRUE) #power
  
  #compute synchrony explained, see Appendix S15 of Sheppard et al (2018)
  mmf<-apply(FUN=mean,X=normforcoh(modval,norm),MARGIN=c(2,3)) #model mean field  
  powmmf<-apply(FUN=mean,X=(Mod(mmf))^2,MARGIN=2,na.rm=TRUE) #power
  res$syncexpl<-(Mod(coher))^2*powmmf
  
  #residuals
  d<-wts[[1]]-modval
  dmf<-apply(FUN=mean,X=d,MARGIN=c(2,3)) #residuals mean field 
  res$resids<-apply(FUN=mean,X=(Mod(dmf))^2,MARGIN=2,na.rm=TRUE) #power
  
  #cross terms
  res$crossterms<-res$sync-res$syncexpl-res$resids
  
  #prepare to get predictors and interaction terms
  h<-list() 
  for (counter in 2:length(wts))
  {
    #start with the wmf of each predictor
    h[[counter-1]]<-apply(FUN=mean,MARGIN=c(2,3),X=wts[[counter]])
    #multiply by coefficients
    h[[counter-1]]<-h[[counter-1]]*
      matrix(rep(object$coefs[,counter-1],each=length(object$times)),
             nrow=length(object$times))
  }
  
  #synchrony explained by each predictor
  for (pc in 1:length(pnames))
  {
    res[,pnames[pc]]<-apply(FUN=mean,MARGIN=2,X=(Mod(h[[pc]]))^2,na.rm=T)
  }
  
  if (length(pnames)>1)
  {
    #now do each set of interactions 
    totints<-numeric(dim(res)[1])
    for (c1 in 1:(length(pnames)-1))
    {
      for (c2 in (c1+1):length(pnames))
      {
        h2<-2*Re(apply(FUN=mean,MARGIN=2,
                       X=h[[c1]]*Conj(h[[c2]]),na.rm=T))
        res[,paste0(pnames[c1],"_",pnames[c2])]<-h2
        totints<-totints+h2
      }
    }
    
    #total interactions
    res[,"interactions"]<-totints
  }
  
  return(res)
}