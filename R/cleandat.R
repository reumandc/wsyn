#Dan has not gone through this yet

#' Clean spatiotemporal data matrices to make them ready for wavelet and other analyses
#'
#' A data cleaning function for applying an optimal Box-Cox transformation, detrending, and standarizing variance
#' 
#' @param indata a locations x time data matrix, or else a time series vector (1 location)
#' @param normalize TRUE or FALSE, use a Box-Cox procedure to find optimal transformation for normalizing data
#' @param each.ts TRUE or FALSE. If TRUE, apply Box-Cox for each timeseries; else apply it for the whole matrix, choosing a single optimal lambda.
#' @param lambdas a vector of lambdas to test; defaults to seq(-10,10, by=0.01)
#' @param detrend TRUE or FALSE, remove a linear trend from each time series; defaults to TRUE
#' @param rescale TRUE or FALSE, if true rescale by dividing each time series by its standard deviation; defaults to TRUE
#' @param do.plot TRUE or FALSE, plot log-likelihood profiles; defaults to FALSE
#' 
#' @return \code{outdat} a list containing the cleaned data, the optimal lambda from the Box-Cox procedure, 
#' and values of detrend and rescale
#' 
#' @note The algorithm determines an optimal transformation parameter (lambda) to apply to the whole dataset
#' by maximizing the summed log-likelihood across all locations. See Sheppared et al. (2015) for details.
#' Procedure developed by Lawrence Sheppard and Daniel Reuman; R code by Jonathan Walter and Lei Zhao.
#' 
#' @author Jonathan Walter, \email{jonathan.walter@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Lei Zhao, \email{leizhao@@ku.edu}
#'
#' @references Sheppard, L.w., et al. (2015) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#'
#' @export  

cleandat<-function(indata, normalize=TRUE, each.ts=FALSE, lambdas=seq(-10,10,by=0.01), detrend=TRUE, rescale=TRUE, do.plot=FALSE){
  
  # library(car)
  library(MASS)
  
  ## Check for missing data
  if(sum(is.na(indata))>0){print("Warning: indata contains missing observations. Note that many methods in Reumannplatz (including all wavelet statistics) are inappropriate for datasets with missing data.")}
  
  bc.trans<-function(y,lambda){ #Set up function for Box-Cox transformation
    if (lambda==0){
      return(log(y))
    }else{
      return((y^lambda-1)/lambda)
    }
  }
  
  out<-list(cleandat=NULL, lambda=NULL, detrend=detrend, rescale=rescale)
  
  if(is.null(nrow(indata))){
    y<<-indata
    x<<-1:length(y)
    if(normalize){
      diffs<-abs(diff(y[order(y)]))
      tsmin<<-min(diffs[diffs !=0], na.rm=T) # set minimum value to smallest difference between consecutive sorted values
      y<<-y-min(y, na.rm=T)+tsmin
      
      like<-boxcox(lm(y~x, na.action=na.exclude),lambda=lambdas, plotit=do.plot)$y
      
      inds<-which(like==max(like, na.rm=T))
      if (length(inds)>1 || length(inds)==0) { stop('Error in Box-Cox routine.') }
      if (inds==length(lambdas) || inds==1) { stop('Make range of lambdas bigger') }
      best.lambda<-lambdas[inds]
      
      y<<-bc.trans(y, best.lambda)
      out$lambda = best.lambda
    }
    if(detrend){
      y<<-residuals(lm(y~x), na.action=na.exclude)#$residuals
    }
    if(rescale){
      y<<-y-mean(y, na.rm=T)
      y<<-y/sd(y, na.rm=T)
    }
    out$cleandat<-y
    rm(x, y, pos=1)
  }else{
    y<-indata
    if(normalize){
      x<<-1:ncol(indata)
      like.mat<-matrix(NA, nrow=nrow(y), ncol=length(lambdas))
      for(mm in 1:nrow(y)){
        #print(mm)
        diffs<-abs(diff(y[mm,][order(y[mm,])]))
        tsmin<-min(diffs[diffs !=0], na.rm=T) # set minimum value to smallest difference between consecutive sorted values
        y[mm,]<-y[mm,]-min(y[mm,], na.rm=T)+tsmin
        
        y.mm<<-y[mm,]
        like.mat[mm,]<-boxcox(lm(y.mm~x, na.action=na.exclude), lambda=lambdas, plotit=do.plot)$y
      }
      ## <<<<<<<<<<<<------------ I added this part
      if(each.ts){ 
        inds<-apply(like.mat, 1, which.max)
        if(any(inds==length(lambdas)) || any(inds==1)){ stop('Make range of lambdas bigger')}
        best.lambda<-lambdas[inds]
        for(mm in 1:nrow(y)){
          y[mm,]<-bc.trans(y[mm,], best.lambda[mm])
        }
        out$lambda<-best.lambda
      }else{
        ###--------------------
        like<-colSums(like.mat)
        
        inds<-which(like==max(like))
        if (length(inds)>1 || length(inds)==0) { stop('Error in Box-Cox routine.') }
        if (inds==length(lambdas) || inds==1) { stop('Make range of lambdas bigger') }
        best.lambda<-lambdas[inds]
        
        for(mm in 1:nrow(y)){
          y[mm,]<-bc.trans(y[mm,], best.lambda)
        }
        out$lambda<-best.lambda
        # rm(x, y.mm, pos=1)
      }
    }
    if(detrend){
      x<-1:ncol(indata)
      for(mm in 1:nrow(y)){
        y[mm,]<-residuals(lm(y[mm,]~x, na.action=na.exclude))#$residuals
      }
    }
    if(rescale){
      for(mm in 1:nrow(y)){
        y[mm,]<-y[mm,]-mean(y[mm,], na.rm=T)
        y[mm,]<-y[mm,]/sd(y[mm,], na.rm=T)
      }
    }
    out$cleandat<-y
    suppressWarnings(rm(x, y.mm, pos=1))
  }
  return(out)
}