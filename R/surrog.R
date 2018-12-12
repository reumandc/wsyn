#' Creates surrogate time series, either Fourier surrogates or amplitude adjusted 
#' Fourier surrogates
#' 
#' For significance testing wavelet coherence and other purposes
#' 
#' @param dat A locations x time matrix of observations (for multiple-time series input), or a single vector
#' @param nsurrogs The number of surrogates to produce
#' @param surrtype Either "fft" (for Fourier surrogates) or "aaft" (for amplitude adjusted Fourier surrogates). 
#' Fourier surrogates are appropriate for time series with normal marginals; otherwise consider aaft surrogates.
#' @param syncpres Logical. TRUE for "synchrony preserving" surrogates (same phase randomizations used for all time 
#' series). FALSE leads to independent phase randomizations for all time series.
#'  
#' @return \code{surrog} returns a list of nsurrogs surrogate datasets 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}; Lawrence Sheppard, \email{lwsheppard@@ku.edu}; 
#' Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @details Fourier surrogates are somewhat faster than \code{aaft} surrogates, and may be much faster when 
#' some of the time series in the data have ties. Prenormalization (e.g., using \code{cleandat}) can 
#' make it possible to use \code{fft} surrogates.
#'
#' @references 
#' Sheppard, LW, et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid pests. Nature Climate 
#' Change. DOI: 10.1038/nclimate2881
#' 
#' Schreiber, T and Schmitz, A (2000) Surrogate time series. Physica D 142, 346-382.
#' 
#' Prichard, D and Theiler, J (1994) Generating surrogate data for time series with several simultaneously measured 
#' variables. Physical Review Letters 73, 951-954.
#'
#' @seealso \code{\link{wpmf}}, \code{\link{coh}}, \code{\link{wlmtest}}, \code{\link{synmat}},
#' \code{browseVignettes("wsyn")}
#'
#' @examples
#' times<-1:100
#' dat<-sin(2*pi*times/10)
#' nsurrogs<-10
#' surrtype<-"fft"
#' syncpres<-TRUE
#' res<-surrog(dat,nsurrogs,surrtype,syncpres)
#' 
#' @export 
#' @importFrom stats qnorm rnorm fft 

surrog<-function(dat,nsurrogs,surrtype,syncpres)
{
  #error check
  wasvect<-FALSE
  if (is.matrix(dat) && dim(dat)[1]>1)
  {
    errcheck_stdat(1:dim(dat)[2],dat,"surrog")
  }else
  {
    if (!is.matrix(dat)){wasvect<-TRUE}
    errcheck_tsdat(1:length(dat),dat,"surrog")
    dat<-matrix(dat, nrow=1, ncol=length(dat))
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
    normquant<-stats::qnorm((1:dim(dat)[2])/(dim(dat)[2]+1))
    
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
      #map time series onto quantiles of a normal, done separately for each time 
      #series in each surrogate, breaking ties randomly
      datorig<-dat
      dat<-list()
      for (counter in 1:nsurrogs)
      {
        datranks<-t(apply(FUN=rank,X=datorig,MARGIN=1,ties.method="random"))
        dat[[counter]]<-matrix(normquant[datranks],nrow(datranks),ncol(datranks))
      }
      
      #get ffts of all time series for all remappings
      fftdat<-list()
      fftmod<-list()
      fftarg<-list()
      for (counter in 1:nsurrogs)
      {
        fftdat[[counter]]<-matrix(complex(real=NA, imaginary=NA), nrow=nrow(datorig), ncol=ncol(datorig))
        for (row in 1:nrow(datorig))
        {
          fftdat[[counter]][row,]<-stats::fft(dat[[counter]][row,])
        }
        fftmod[[counter]]<-Mod(fftdat[[counter]])
        fftarg[[counter]]<-Arg(fftdat[[counter]])
      }
      
      #randomize phases and inverse transform
      mpdres<-list()
      for(counter in 1:nsurrogs)
      {
        # get and apply random phases for the current surrogate
        if (syncpres)
        {
          #synchrony preserving surrogates only need one set of phase pertubations, used for all time series
          h<-Arg(stats::fft(stats::rnorm(ncol(datorig))))
          randomizedphases<-
            (matrix(rep(h, times=nrow(datorig)), nrow(datorig), ncol(datorig), byrow=TRUE)+fftarg[[counter]]) %% (2*pi)
        }else
        {
          #need separate independent phase perturbations for each time series
          h<-matrix(stats::rnorm(ncol(datorig)*nrow(datorig)),nrow(datorig),ncol(datorig))
          randomizedphases<-(fftarg[[counter]]+t(apply(X=h,MARGIN=1,FUN=function(x){Arg(stats::fft(x))}))) %% (2*pi)
        }
        mpdres[[counter]]<-matrix(complex(modulus=fftmod[[counter]], 
                                          argument=randomizedphases),nrow(datorig), ncol(datorig))
        
        # inverse transform
        for(row in 1:nrow(mpdres[[counter]]))
        {
          mpdres[[counter]][row,]<-fft(mpdres[[counter]][row,], inverse=T)/(ncol(mpdres[[counter]]))
        }
        mpdres[[counter]]<-Re(mpdres[[counter]])
      }

      #get orders for remapped data, same as orders of ranks of remapped data      
      datorders<-list()
      for (counter in 1:nsurrogs)
      {
        datorders[[counter]]<-t(apply(FUN=order,X=dat[[counter]],MARGIN=1))
      }

      #remap results back
      res<-list()
      for (counter1 in 1:nsurrogs)
      {
        res[[counter1]]<-NA*datorig
        
        thissurrog<-mpdres[[counter1]]
        thissurrogrk<-t(apply(FUN=rank,X=thissurrog,MARGIN=1))
        
        for (counter2 in 1:nrow(datorig))
        {
          res[[counter1]][counter2,]<-
            datorig[counter2,datorders[[counter1]][counter2,thissurrogrk[counter2,]]]
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