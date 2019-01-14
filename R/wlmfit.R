#' Fits a wavelet linear model
#' 
#' Stripped down internal function for doing the fitting
#' 
#' @param wts List of normalized transforms, normalized as specified in \code{norm}. Each entry a locations x time x timescales array of transforms. The first is the response variable, others are the predictors.
#' @param norm The normalization that was used. One of "none", "powall", "powind". See details. 
#'
#' @return \code{wlmfit} returns a list with these elements:
#' \item{coefs}{Model coefficients}
#' \item{modval}{The right had side of the model}
#' \item{coher}{Appropriately normalized coherence of the model and response variable}
#' 
#' @details Only \code{norm="powall"} works now, other options throw an error.
#' 
#' @note Internal function, no error checking done.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology. In press.

wlmfit<-function(wts,norm)
{
  #This looks like an error check, but it will be removed when the other normalizations
  #are added
  if (norm!="powall")
  {
    stop("Error in wlmfit: only the powall option for norm is implemented so far")
  }
  
  #setup - wts[[i]] is locs by times by timescales
  N<-dim(wts[[1]])[1] #number of locations
  Ti<-dim(wts[[1]])[2] #length of time series
  V<-length(wts)-1 #number of predictor variables
  lents<-dim(wts[[1]])[3] #number of timescales
  
  #do the fitting 
  coefs<-as.data.frame(matrix(NA*numeric(1),lents,V))
  X<-matrix(complex(real=NA,imaginary=NA),N*Ti,V)
  for (tscounter in 1:lents)
  {
    #make the design matrix
    for (vcounter in 1:V)
    {
      X[,vcounter]<-as.vector(wts[[vcounter+1]][,,tscounter])
    }
    
    #make the response variable
    y<-as.vector(wts[[1]][,,tscounter])
    
    #get rid of non-finite entries and do the qr.solve call
    inds<-which(is.finite(y))
    tsres<-qr.solve(X[inds,],y[inds])
    
    #store the result in the desired format
    coefs[tscounter,]<-tsres
  }
  
  #get modvals and coher
  modval<-0*wts[[1]] #holder for the model, right hand side of regression equation
  for (vcounter in 1:V)
  {
    modval<-modval+wts[[vcounter+1]]*array(rep(coefs[,vcounter],each=N*Ti),dim=dim(wts[[1]]))
  }
  coher<-apply(X=wts[[1]]*Conj(normforcoh(modval,norm)),FUN=mean,MARGIN=3,na.rm=TRUE)
  
  return(list(coefs=coefs,modval=modval,coher=coher))
}

