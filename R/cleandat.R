#' Clean (spatio)temporal data matrices to make them ready for analyses using the \code{wsyn} package
#'
#' A data cleaning function for optimal Box-Cox transformation, detrending, standarizing variance, de-meaning
#' 
#' @param dat A locations x time data matrix, or a time series vector (for 1 location)
#' @param times The times of measurement
#' @param clev The level of cleaning to do, 1 through 4. See details. 
#' @param lambdas A vector of lambdas to test for optimal Box-Cox transformation, if Box-Cox is performed. Ignored for \code{clev<4}. Defaults to seq(-10,10, by=0.01). The best of these is used.
#' @param mints If \code{clev==4}, then time series are shifted to have this minimum value. Default NA means use the smallest difference between consecutive, distinct sorted values.
#' 
#' @return \code{cleandat} returns a list containing the cleaned data, \code{clev}, and the optimal lambdas from the 
#' Box-Cox procedure (\code{NA} for \code{clev<4}, see Details).
#' 
#' @details NAs, Infs, etc. in \code{dat} trigger an error. If \code{clev==1}, time series are (individually) 
#' de-meaned. If \code{clev==2}, time series are (individually) linearly detrended and de-meaned. If \code{clev==3}, 
#' time series are (individually) linearly detrended and de-meaned, and variances are standardized to 1. If 
#' \code{clev==4}, an optimal Box-Cox normalization procedure is applied to each time series (individually), and 
#' they are linearly detrended, de-meaned, and variances are standardized to 1. Constant time series and perfect 
#' linear trends trigger an error for \code{clev>=3}. If \code{clev==4} and the optimal \code{lambda} for one or 
#' more time series is a boundary case or if there is more than one optimal lambda, it triggers a warning. A 
#' wider range of \code{lambda} should be considered in the former case. Before the Box-Cox procedure, the 
#'  
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Lei Zhao, \email{leizhao@@ku.edu}
#'
#' @references 
#' Box, GEP and Cox, DR (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211–252.
#' Venables, WN and Ripley, BD (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' Sheppard, LW, et al. (2015) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @examples 
#' #Don't have any yet but need some
#' 
#' @export  

cleandat<-function(dat,times,clev,lambdas=seq(-10,10,by=0.01),mints=NA)
{
  #error checking
  if (!(clev %in% c(1,2,3,4)))
  {
    stop("Error in cleandat: clev must be 1, 2, 2, or 4")
  }
  if (clev==4 && is.finite(mints) && mints<=0)
  {
    stop("Error in cleandat: mints, if specified and if clev is 4, must be positive")
  }
  errcheck_times(times,"cleandat")
  if (!is.numeric(dat))
  {
    stop("Error in cleandat: dat must be numeric")
  }
  wasvect<-FALSE
  if (!is.matrix(dat))
  {
    wasvect<-TRUE
    dat<-matrix(dat,1,length(dat))
  }
  if (length(times)!=dim(dat)[2])
  {
    stop("Error in cleandat: length of dat and times must be equal")
  }
  if (!all(is.finite(dat)))
  {
    stop("Error in cleandat: dat must not contain NAs, NaNs, Infs")
  }
  #error check for perfect linear trends
  if (clev>=3)
  {
    for (counter in 1:dim(dat)[1])
    {
      thisrow<-dat[counter,]
      if (isTRUE(all.equal(sd(residuals(lm(thisrow~times))),0)))
      {
        stop("Error in cleandat: cannot perform clev 3 cleaning on time series that are constant or a perfect linear trend")
      }
    }
  }
  
  cdat<-dat
  optlambdas<-NA*numeric(1)
  if (clev==1)
  {
    #de-mean only
    for (crow in 1:dim(cdat)[1])
    {
      cdat[crow,]<-cdat[crow,]-mean(cdat[crow,])
    }
  }
  
  if (clev==4)
  {
    #optimal Box-Cox
    optlambdas<-NA*numeric(dim(cdat)[1])
    for (crow in 1:dim(cdat)[1])
    {
      thisrow<-cdat[crow,]
        
      #set minimum value to smallest difference between consecutive sorted values
      if (!is.finite(mints))
      {
        diffs<-diff(sort(thisrow))
        mints<-min(diffs[diffs!=0]) 
      }
      thisrow<-thisrow-min(thisrow)+mints
      
      alllikes<-NA*numeric(length(lambdas))
      for (clam in 1:length(lambdas))
      {
        alllikes[clam]<-boxcoxloglike(lambdas[clam],thisrow)
      }
      plot(lambdas,alllikes,type='l')
      inds<-which(alllikes==max(alllikes))
      if (length(inds)>1)
      {
        warning("Warning from cleandat: more than one optimal value of lambda, the first was used")
        inds<-inds[1]
      }
      if (inds==1 || inds==length(lambdas))
      {
        warning("Warning from cleandat: boundary optimal lambda, use wider range")
      }
      optlambdas[crow]<-lambdas[inds]
      cdat[crow,]<-bctrans(thisrow,optlambdas[crow])
    }
  }
  
  if (clev>=2)
  {
    #detrend and de-mean
    for (crow in 1:dim(cdat)[1])
    {
      thisrow<-cdat[crow,]
      cdat[crow,]<-residuals(lm(thisrow~times))
    }
  }
  
  if (clev>=3)
  {
    #standardize variance to 1
    for (crow in 1:dim(dat)[1])
    {
      cdat[crow,]<-cdat[crow,]/sd(cdat[crow,])
    }
  }
  
  if (wasvect)
  {
    cdat<-as.vector(cdat)  
  }
  
  return(list(cdat=cdat,clev=clev,optlambdas=optlambdas))
}