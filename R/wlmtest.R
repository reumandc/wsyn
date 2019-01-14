#' Statistical comparison of wavelet linear models
#' 
#' Compares a wavelet linear model with a nested model. Also the generator function for 
#' the \code{wlmtest} class.
#' 
#' @param wlmobj A \code{wlm} object
#' @param drop Either names or indices of variables in \code{wlmobj$dat} that are being 
#' dropped to form the simpler, nested model. The first variable in \code{wlmobj$dat}, 
#' which is the response, is not allowed here.
#' @param sigmethod Method for significance testing. One of "\code{fft}", "\code{aaft}", "\code{fast}". See details.
#' @param nrand The number of randomizations to do for significance
#' 
#' @return \code{wlmtest} returns an object of class \code{wlmtest}. Slots are:
#' \item{wlmobj}{The input}
#' \item{drop}{The input}
#' \item{signif}{A list with information from the significance testing. Elements are 
#' \code{sigmethod} (the input), \code{coher} and \code{scoher}. See details.}
#' \item{ranks}{A list with ranking information for \code{signif}. \code{NA} until 
#' \code{plotrank} or \code{bandtest} is called.}
#' \item{bandp}{A data frame containing results of computing significances across 
#' timescale bands. Empty on an initial call to \code{wlmtest}, filled in by the function 
#' \code{bandtest}. See details.}
#' 
#' @details The slot \code{signif} provides the core information on significance. 
#' If \code{sigmethod} is not "\code{fast}", then \code{signif$coher} is the same as 
#' \code{wlmobj$coher}, and \code{signif$scoher} is a matrix of dimensions \code{nrand} by 
#' \code{length(signif$coher)} with rows equal to coherences between refitted models and the 
#' response-variable transforms, for datasets where the variables specified in \code{drop} have
#' been replaced by surrogates. Normalization as specified in \code{norm} is used. The type 
#' of surrogate used (Fourier surrogates or amplitude adjusted Fourier surrogates, see 
#' \code{surrog}) is determined by \code{sigmethod} ("\code{fft}" or "\code{aaft}"). 
#' Synchrony-preserving surrogates are used. A variety of statements of significance (or lack 
#' thereof) can be made by comparing \code{signif$coher} with \code{signif$scoher} (see the 
#' \code{plotmag}, \code{plotrank}, and \code{bandtest} methods
#' for the \code{wlmtest} class). If \code{sigmethod} is 
#' "\code{fast}", a fast algorithm of Lawrence Sheppard is used which is a generalization 
#' to wavelet linear models of the fast algorithm for coherence described in Sheppard et al (2017). 
#' In that case
#' \code{signif$coher} can be compared to \code{signif$scoher} to make significance 
#' statements about the coherence in exactly the same way, but \code{signif$coher} will no
#' longer precisely equal \code{wlmobj$coher}, and \code{wlmobj$coher} should not be compared 
#' directly to \code{signif$scoher}. Statements about significance of the coherence 
#' should be made using \code{signif$coher} and \code{signif$scoher}, whereas \code{wlmobj$coher}
#' should be used whenever the actual value of the coherence is needed. 
#' 
#' The slots \code{ranks} and \code{bandp} are empty on an initial call to \code{wlmtest}. 
#' They are made to compute and hold 
#' aggregate significance results over any timescale band of choice. These are filled in
#' when needed by other methods, see \code{plotrank} and \code{bandtest}. 
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' Sheppard, L.W., et al. (2017) Rapid surrogate testing of wavelet coherences. European Physical 
#' Journal, Nonlinear and Biomedical Physics, 5, 1. DOI: 10.1051/epjnbp/2017000
#' 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology. In press.
#'
#' @seealso \code{\link{wlm}}, \code{\link{plotrank}}, \code{\link{bandtest}}, \code{\link{coh}}, 
#' \code{\link{wlmtest_methods}}, \code{browseVignettes("wsyn")}
#'
#' @examples
#' times<-1:30
#' dat<-list(v1=matrix(rnorm(300),10,30),v2=matrix(rnorm(300),10,30),v3=matrix(rnorm(300),10,30),
#'           v4=matrix(rnorm(300),10,30),v5=matrix(rnorm(300),10,30))
#' dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
#' resp<-1
#' pred<-2:3
#' norm<-"powall"
#' wlmobj<-wlm(dat,times,resp,pred,norm)
#' drop<-3
#' sigmethod<-"fft"
#' res<-wlmtest(wlmobj,drop,sigmethod,nrand=10)
#' 
#' @export

wlmtest<-function(wlmobj,drop,sigmethod,nrand=1000)
{
  #**error checking
  if (wlmobj$norm!="powall")
  { #we can assume norm is powall, powind or none, since it is in a wlm object
    #this error check will be removed when the other options are implemented
    stop("Error in wlmtest: this value of norm not implemented yet")
  }
  if (!(all(drop %in% 2:length(wlmobj$dat)) || 
        all(drop %in% names(wlmobj$dat)[2:length(wlmobj$dat)])))
  {
    stop("Error in wlmtest: drop must contain names or indices of predictors used in fitting wlmobj")
  }
  if (length(unique(drop))!=length(drop))
  {
    stop("Error in wlmtest: drop must not have repeat entries")
  }
  if (!(sigmethod %in% c("fft","aaft","fast")))
  {
    stop("Error in wlmtest: bad value for sigmethod")
  }
  
  #**convert drop from names to indices if necessary
  origdrop<-drop
  if (all(drop %in% names(wlmobj$dat)[2:length(wlmobj$dat)]))
  {
    drop<-which(names(wlmobj$dat)[2:length(wlmobj$dat)] %in% drop)+1
  }
  
  #**fast algorithm
  if (sigmethod=="fast")
  {
    stop("Error in wlmtest: fast algorithm not implemented yet")
    
    #***DAN: fill in
    #***Dont forget to include sigmethod in signif
    
    #prepare result  
    result<-list(wlmobj=wlmobj,drop=origdrop,signif=signif,ranks=NA,bandp=NA)
    class(result)<-c("wlmtest","list")
    return(result)    
  }
  
  #**slow algorithm
  
  #*get joint surrogates for dropped variables
  cddat<-wlmobj$dat[[drop[1]]]
  if (length(drop)>1)
  {
    for (counter in 2:length(drop))
    { #put the data together to get joint surrogates
      cddat<-rbind(cddat,wlmobj$dat[[drop[counter]]])
    } 
  } 
  scddat<-surrog(cddat,nrand,sigmethod,TRUE)
  
  #*do transforms
  wscddat<-lapply(FUN=function(x)
                        {
                          warray(x,times=wlmobj$times,
                                 scale.min=wlmobj$wtopt$scale.min,
                                 scale.max.input=wlmobj$wtopt$scale.max.input,
                                 sigma=wlmobj$wtopt$sigma,f0=wlmobj$wtopt$f0)$wavarray
                        },X=scddat)
  
  #*refit for each surrogate, keeping coherences
  coher<-wlmobj$coher
  scoher<-matrix(NA*complex(real=length(coher)*nrand,
                            imaginary=length(coher)*nrand),
                 nrand,length(coher))
  wts<-wlmobj$wts
  for (scounter in 1:nrand)
  { 
    #replace the appropriate entries of wts by surrogate transforms
    for (dcounter in 1:length(drop))
    {
      st<-(dcounter-1)*dim(wlmobj$dat[[1]])[1]+1
      en<-dcounter*dim(wlmobj$dat[[1]])[1]
      wts[[drop[dcounter]]]<-normforcoh(wscddat[[scounter]][st:en,,],wlmobj$norm)
    }
    #refit and just keep the coher result
    scoher[scounter,]<-wlmfit(wts,wlmobj$norm)$coher 
  }
  signif<-list(sigmethod=sigmethod,coher=coher,scoher=scoher)
  
  #prepare result  
  result<-list(wlmobj=wlmobj,drop=origdrop,signif=signif,ranks=NA,bandp=NA)
  class(result)<-c("wlmtest","list")
  return(result)    
}

