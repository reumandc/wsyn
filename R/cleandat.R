#' Clean (spatio)temporal data matrices to make them ready for analyses using the \code{wsyn} package
#'
#' A data cleaning function for optimal Box-Cox transformation, detrending, standarizing variance, 
#' de-meaning
#' 
#' @param dat A locations x time data matrix, or a time series vector (for 1 location)
#' @param times The times of measurement, spacing 1
#' @param clev The level of cleaning to do, 1 through 5. See details. 
#' @param lambdas A vector of lambdas to test for optimal Box-Cox transformation, if Box-Cox is 
#' performed. Ignored for \code{clev<4}. Defaults to seq(-10,10, by=0.01). See details.
#' @param mints If \code{clev} is 4 or 5, then time series are shifted to have this minimum value 
#' before Box-Cox transformation. Default NA means use the smallest difference between consecutive, 
#' distinct sorted values. NaN means perform no shift.
#' 
#' @return \code{cleandat} returns a list containing the cleaned data, \code{clev}, and the optimal 
#' lambdas from the Box-Cox procedure (\code{NA} for \code{clev<4}, see details).
#' 
#' @details NAs, Infs, etc. in \code{dat} trigger an error. If \code{clev==1}, time series are (individually) 
#' de-meaned. If \code{clev==2}, time series are (individually) linearly detrended and de-meaned. If \code{clev==3}, 
#' time series are (individually) linearly detrended and de-meaned, and variances are standardized to 1. If 
#' \code{clev==4}, an optimal Box-Cox normalization procedure is applied jointly to all time series (so the same
#' Box-Cox transformation is applied to all time series after they are individually shifted depending on the value
#' of \code{mints}). Transformed time series are then individually linearly detrended, de-meaned, and variances are
#' standardized to 1. If \code{clev==5}, an optimal Box-Cox normalization procedure is applied to each time series 
#' individually (again after individually shifting according to \code{mints}), and transformed time series are then 
#' individually linearly detrended, de-meaned, and variances are standardized to 1. Constant time series and perfect 
#' linear trends trigger an error for \code{clev>=3}. If \code{clev>=4} and the optimal \code{lambda} for one or 
#' more time series is a boundary case or if there is more than one optimal lambda, it triggers a warning. A wider 
#' range of \code{lambda} should be considered in the former case. 
#'  
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; 
#' Daniel Reuman, \email{reuman@@ku.edu}; Lei Zhao, \email{lei.zhao@@cau.edu.cn}
#'
#' @references 
#' Box, GEP and Cox, DR (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211–252.
#'
#' Venables, WN and Ripley, BD (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' Sheppard, LW, et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @seealso \code{\link{wt}}, \code{\link{wmf}}, \code{\link{wpmf}}, \code{\link{coh}}, \code{\link{wlm}}, 
#' \code{\link{wlmtest}}, \code{\link{clust}}, \code{browseVignettes("wsyn")}
#' 
#' @examples 
#' times<-1:100
#' dat<-rnorm(100)
#' res1<-cleandat(dat,times,1) #this removes the mean
#' res2<-cleandat(dat,times,2) #detrends and removes the mean
#' res3<-cleandat(dat,times,3) #variances also standardized
#' res4<-cleandat(dat,times,4) #also joint Box-Cox applied
#' res5<-cleandat(dat,times,5) #1-3, also indiv Box-Cox
#' 
#' @export  
#' @importFrom stats lm residuals sd
#' @importFrom MASS boxcox

cleandat<-function(dat,times,clev,lambdas=seq(-10,10,by=0.01),mints=NA)
{
  #error checking
  if (!(clev %in% 1:5))
  {
    stop("Error in cleandat: clev must be 1, 2, 3, 4, or 5")
  }
  if (clev %in% 4:5 && !(is.na(mints) || is.nan(mints) || (is.finite(mints) && mints>0)))
  {
    stop("Error in cleandat: if clev is 4 or 5, mints must be NA, NaN, or a positive number")
  }
  errcheck_times(times,"cleandat")
  if (inherits(dat,"data.frame"))
  {
    stop("Error in cleandat: dat must be a vector or matrix, not a dataframe")
  }
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
      if (isTRUE(all.equal(sd(stats::residuals(stats::lm(thisrow~times))),0)))
      {
        stop("Error in cleandat: cannot perform clev 3 or greater cleaning on time series that are constant or a perfect linear trend")
      }
    }
  }

  cdat<-dat
  optlambdas<-NA*numeric(1)
  
  #de-mean only
  if (clev==1)
  {
    for (crow in 1:dim(cdat)[1])
    {
      cdat[crow,]<-cdat[crow,]-mean(cdat[crow,])
    }
  }
  
  #optimal Box-Cox done jointly on all time series
  if (clev==4)
  {
    #set minimum value for first time series
    thisrow<-cdat[1,]
    thisrow<-setmints(thisrow,mints)    
    
    #do Box-Cox for first time series
    bxcxres<-MASS::boxcox(thisrow~times,lambda=lambdas,plotit=FALSE,interp=FALSE)
    xfin<-bxcxres$x
    yfin<-bxcxres$y
    
    #now go through the rest of the time series
    if (dim(cdat)[1]>1)
    {
      for (crow in 2:dim(cdat)[1])
      {
        #set minimum value for this row
        thisrow<-cdat[crow,]
        thisrow<-setmints(thisrow,mints)
        
        #do Box-Cox for this row
        bxcxres<-MASS::boxcox(thisrow~times,lambda=lambdas,plotit=FALSE,interp=FALSE)
        if (!isTRUE(all.equal(xfin,bxcxres$x))){stop("Error in cleandat: boxcox problem with independent variable")}
        yfin<-yfin+bxcxres$y
      }
    }
    
    #warnings from badly behaved log likelihoods, then set the optimal lambda
    inds<-which(yfin==max(yfin))
    #plot(xfin,yfin,type='l')
    if (length(inds)>1)
    {
      warning("Warning from cleandat: more than one optimal value of lambda, the first was used")
      inds<-inds[1]
    }
    if (inds==1 || inds==length(lambdas))
    {
      warning("Warning from cleandat: boundary optimal lambda, use wider range")
    }
    optlambdas<-xfin[inds]
    
    #now do the transformations
    for (crow in 1:dim(cdat)[1])
    {
      thisrow<-cdat[crow,]
      thisrow<-setmints(thisrow,mints)    
      cdat[crow,]<-bctrans(thisrow,optlambdas)
    }
  }
  
  #optimal Box-Cox done individually on time series 
  if (clev==5)
  {
    optlambdas<-NA*numeric(dim(cdat)[1])
    for (crow in 1:dim(cdat)[1])
    {
      thisrow<-cdat[crow,]
        
      #set minimum value
      thisrow<-setmints(thisrow,mints)
      
      bxcxres<-MASS::boxcox(thisrow~times,lambda=lambdas,plotit=FALSE,interp=FALSE)
      inds<-which(bxcxres$y==max(bxcxres$y))
      if (length(inds)>1)
      {
        warning("Warning from cleandat: more than one optimal value of lambda, the first was used")
        inds<-inds[1]
      }
      if (inds==1 || inds==length(lambdas))
      {
        warning("Warning from cleandat: boundary optimal lambda, use wider range")
      }
      optlambdas[crow]<-bxcxres$x[inds]
      cdat[crow,]<-bctrans(thisrow,optlambdas[crow])
    }
  }
  
  #detrend and de-mean 
  if (clev>=2)
  {
    for (crow in 1:dim(cdat)[1])
    {
      thisrow<-cdat[crow,]
      cdat[crow,]<-stats::residuals(stats::lm(thisrow~times))
    }
  }
  
  #standardize variance to 1 
  if (clev>=3)
  {
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