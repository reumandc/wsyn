#' Creates surrogate time series, either Fourier surrogates or amplitude adjusted 
#' Fourier surrogates
#' 
#' For significance testing wavelet coherence and other purposes
#' 
#' @param dat A locations x time matrix of observations (for multiple-time series input), or a single vector
#' @param nsurrogs The number of surrogates to produce
#' @param surrtype Either "fft" (for Fourier surrogates) or "aaft" for amplitude adjusted Foutier surrogates. Fourier surrogates are appropriate for time series with normal marginals; otherwise consider aaft surrogates.
#' @param syncpres Logical. TRUE for "synchrony preserving" surrogates (same phase randomizations used for all time series). FALSE leads to independent phase randomizations for all time series.
#'  
#' @return \code{surrog} returns a list of nsurrogs surrogate datasets 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; 
#' Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references 
#' Sheppard, LW, et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' Schreiber, T and Schmitz, A (2000) Surrogate time series. Physica D 142, 346-382.
#' Prichard, D and Theiler, J (1994) Generating surrogate data for time series with several simultaneously measured variables. Physical Review Letters 73, 951-954.
#'     
#' @examples
#' #Not yet written, but need some
#' 
#' @export 

surrog<-function(dat,nsurrogs,surrtype,syncpres)
{
  #error check
  wasvect<-FALSE
  if (is.matrix(dat))
  {
    errcheck_stdat(1:dim(dat)[2],dat,"surrog")
  }else
  {
    errcheck_tsdat(1:length(dat),dat,"surrog")
    dat<-matrix(dat, nrow=1, ncol=length(dat))
    wasvect<-TRUE
  }
  if (!(surrtype %in% c("fft","aaft")))
  {
    stop("Error in surrog: bad value for surrtype")
  }

  #fft surrogates
  if (surrtype=="fft")
  {
    #do the surrogates
    res<-fftsurrog(dat,nsurrogs,syncpres)
    
    #if dat was a vector, make output same format
    if (wasvect)
    {
      for (n in 1:length(res))
      {
        res[[n]]<-as.vector(res[[n]])
      }
    }

    return(res)
  }
  
  #aaft surrogates  
  if (surrtype=="aaft")
  {
    #get appropriate quantiles of a standard normal
    normquant<-qnorm((1:dim(dat)[2])/(dim(dat)[2]+1))
    
    #find out of there are ties
    areties<-FALSE
    for (counter in 1:dim(dat)[1])
    {
      if (length(unique(dat[counter,]))<dim(dat)[2])
      {
        areties<-TRUE
        break
      }
    }

    #cases with ties are handled differently, and less efficiently by necessity    
    if (areties)
    {
      stop("Error in surrog: aaft surrogates not implemented for time series with ties yet")
      #Q - break ties and map separately for each time series? or by combining all values into one pot?
      #A - separately
      
      # if there are ties
      #   break the ties randomly (use the rank function) while mapping onto quantiles of a normal (this has to be done once for each surrogate, since it is random)
      #   randomize the phases of each of these
      #   inverse transform
      #   remap, undoing the first step (so you will have to retain each mapping)
      # Probably these two alternatives have to be done separately, because for the second you have to fft each random mapping of the data to normal quantiles
      # Note: the rank function will come in handy because it can randomly break ties or not, according to an option
      # Note: the case of ties is much less efficient because two additional steps have to be done separately for all surrogs
    }else
    {
      #map each time series (separately) onto the quantiles of a normal dist
      datorig<-dat
      datranks<-t(apply(FUN=rank,X=dat,MARGIN=1))
      dat<-matrix(normquant[datranks],nrow(datranks),ncol(datranks))
      
      #apply fft surrogates
      mpdres<-fftsurrog(dat,nsurrogs,syncpres)
     
      #remap results back 
      res<-list()
      datorders<-t(apply(FUN=order,X=dat,MARGIN=1))
      for (counter1 in 1:length(mpdres))
      {
        res[[counter1]]<-NA*dat
        
        thissurrog<-mpdres[[counter1]]
        thissurrogrk<-t(apply(FUN=rank,X=thissurrog,MARGIN=1))
        
        for (counter2 in 1:dim(dat)[1])
        {
          res[[counter1]][counter2,]<-datorig[counter2,datorders[counter2,thissurrogrk[counter2,]]]
        }  
      }
      
      #if dat was a vector, make output same format
      if (wasvect)
      {
        for (n in 1:length(res))
        {
          res[[n]]<-as.vector(res[[n]])
        }
      }
      
      return(res)
    }
  }
}