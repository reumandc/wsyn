#' Wavelet linear models
#' 
#' Fits wavelet linear models. Also the generator function of the \code{wlm} class, which
#' inherits from the \code{list} class.
#' 
#' @param dat A list of matrices representing the data (or in the case of one location, a list of
#' vectors). All the same dimensions (respectively, lengths)
#' @param times The times at which measurements were made, spacing 1
#' @param resp Index in dat for the response variable of the model
#' @param pred Vector of indices in dat for the predictor variables of the model; must differ from \code{resp}
#' @param norm The normalization of wavelet transforms to use. One of "none", "powall", "powind". See details. 
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation that will be examined. Note that if this is set too high relative to the length of the timeseries it will be truncated.
#' @param sigma The ratio of each time scale examined relative to the next timescale. Greater than 1. 
#' @param f0 The ratio of the period of fluctuation to the width of the envelope
#' 
#' @return \code{wlm} returns an object of class \code{wlm}. Slots are:
#' \item{dat}{The input data list, but reordered and subsetted so the response is first and only used predictors are included}
#' \item{times}{The times associated with the data}
#' \item{norm}{The input}
#' \item{wtopt}{The inputted wavelet transform options scale.min, scale.max.input, sigma, f0 in a list}
#' \item{wts}{List of transforms, normalized as specified in \code{norm}. Same length as the output \code{dat}, each entry a locations x time x timescales array of transforms.}
#' \item{timescales}{The timescales associated with the wavelet transforms of the data}
#' \item{coefs}{A list (data frame, actually) of complex vectors, each of length the same 
#' as \code{timescales}. These are the model coefficients (which depend on timescale), and 
#' correspond to the \code{wts}.}
#' \item{modval}{The model values.}
#' \item{coher}{Appropriately normalized version of coherence of the model and response transforms. See details.}
#' 
#' @details Normalization is as specified in the documentation for \code{coh}, HOWEVER, only
#' the "\code{powall}" option is currently implemented, other choices throw an error. Details 
#' are specified in appendices S7 and S9 of Sheppard et al, 2018. The output \code{modval}
#' is v in appendix S7, and \code{coefs} are the betas in equation 12 in that appendix.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology 15, e1006744. doi: 10.1371/journal.pcbi.1006744
#' 
#' @seealso \code{\link{wlm_methods}}, \code{\link{wlmtest}}, \code{\link{syncexpl}}, \code{\link{predsync}}, 
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-1:30
#' dat<-list(v1=matrix(rnorm(300),10,30),v2=matrix(rnorm(300),10,30),v3=matrix(rnorm(300),10,30),
#'           v4=matrix(rnorm(300),10,30),v5=matrix(rnorm(300),10,30))
#' dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
#' resp<-2
#' pred<-c(1,3,4)
#' norm<-"powall"
#' res<-wlm(dat,times,resp,pred,norm)
#' 
#' @export

wlm<-function(dat,times,resp,pred,norm,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
{
  #**error checking
  errcheck_times(times,"wlm")
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"wlm")
  if (!(norm %in% c("powall","powind","none")))
  {
    stop("Error in wlm: inappropriate value of norm")
  }
  if (norm %in% c("powind","none"))
  {
    stop("Error in wlm: this value of norm not implemented yet")
  }
  if (!inherits(dat,"list"))
  {
    stop("Error in wlm: dat must be a list")
  }
  if (!all(c(resp,pred) %in% 1:length(dat)))
  {
    stop("Error in wlm: resp and pred must be indices of dat")
  }
  if (length(resp)!=1)
  {
    stop("Error in wlm: you can only have one response")
  }
  if (length(pred)<1)
  {
    stop("Error in wlm: you must have at least one predictor")
  }
  if (resp %in% pred)
  {
    stop("Error in wlm: resp cannot be in pred")
  }
  
  #**rearrange dat according to resp and pred, getting the dat output
  dat<-dat[c(resp,pred)]
  
  #**more error checking
  wasvect<-FALSE
  if (is.matrix(dat[[1]]) && dim(dat[[1]])[1]>1)
  {
    d<-dim(dat[[1]])
    for (counter in 1:length(dat))
    {
      if (!isTRUE(all.equal(dim(dat[[counter]]),d)))
      {
        stop("Error in wlm: all data matrices must be the same dimensions")
      }
      errcheck_stdat(times,dat[[counter]],"wlm")
    }
  }else
  {
    if (!is.matrix(dat[[1]])){wasvect<-TRUE}
    for (counter in 1:length(dat))
    {
      errcheck_tsdat(times,as.vector(dat[[counter]]),"wlm") 
      dat[[counter]]<-matrix(dat[[counter]],1,length(times))      
    }
  }

  #**get wavelet transforms and normalize, getting the wts output
  wts<-lapply(FUN=warray,X=dat,times=times,scale.min=scale.min,
         scale.max.input=scale.max.input,sigma=sigma,f0=f0)
  timescales<-wts[[1]]$timescales
  wts<-lapply(X=wts,FUN=function(x){normforcoh(x$wavarray,norm)})
  
  #**do the fitting by call to the internal wlmfit, getting coefs, modval, coher
  h<-wlmfit(wts,norm)
  coefs<-h$coefs
  names(coefs)<-names(dat)[2:length(dat)]

  #**prepare result  
  if (wasvect){dat<-lapply(FUN=as.vector,X=dat)}
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
             sigma=sigma,f0=f0)
  result<-list(dat=dat,times=times,norm=norm,wtopt=wtopt,
               wts=wts,timescales=timescales,
               coefs=coefs,modval=h$modval,coher=h$coher)
  class(result)<-c("wlm","list")
  return(result)    
}
