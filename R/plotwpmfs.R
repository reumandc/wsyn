#' Plots wavelet phasor means fields of clusters in a \code{clust} object
#' 
#' Plots wavelet phasor means fields of clusters in a \code{clust} object. Generates them and stores 
#' them in the \code{clust} object if they do not already exist.
#' 
#' @param object A \code{clust} object
#' @param spltlvl The level of split to use. Must be between 1 and \code{length(object$clusters)}. The former 
#' corresponds to all locations (prior to any splitting) and the latter to the final set of clusters.
#' @param filename The filename (without extension) to use for saving plots. One plot for each cluster,
#' names (if given) formed by appending the cluster number to the end of filename and then adding an 
#' extension. Default NA uses the default plotting device.
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation guaranteed to be examined
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelop
#' @param sigmethod Method for significance testing the wmpf, one of \code{quick}, \code{fft}, \code{aaft} (see details)
#' @param nrand The number of randomizations to be used for significance testing
#' 
#' @return \code{plotwpmfs} returns a new cluster object for which the \code{wpmfs} slot has been 
#' amended. That slot is a list of length \code{length(object$clusters)}. Each entry of the slot is
#' a list of length equal to the number of clusters at that clustering level, containing \code{wpmf}
#' objects. One call to \code{plotwpmfs} only fills in the \code{wpmf} objects for \code{spltlvl}.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' #Not written yet but need some
#'  
#' @export

plotwpmfs<-function(object,...)
{
  UseMethod("plotwpmfs",object)
}

#' @rdname plotwpmfs
#' @export
plotwpmfs.clust<-function(object,spltlev=length(object$clusters),filename=NA)
{
  #error check
  if (!is.numeric(spltlev) || length(spltlev)!=1 || !(spltlev %in% 1:length(object$clusters)))
  {
    stop("Error in plotwpmfs: bad value for spltlev")
  }
  if (!is.na(filename) && (!is.character(filenames) || length(filename)!=1))
  {
      stop("Error in plotwpmfs: bad value for filename")
  }

  #if the wpmfs have not already been computed, add them via addwpmfs
  if (is.na(object$wpmfs))
  {
    object$wpmfs<-as.list(rep(NA,length(object$clusters)))
  }
  if (is.na(object$wpmfs[[spltlev]]))
  {
    h<-list()
    for (counter in 1:max(object$clusters[[spltlev]]))
    {
      h[[counter]]<-wpmf(dat=object$dat[object$clusters[[spltlev]]==counter,],
           times=object$times,
           scale.min=scale.min,
           scale.max.output=scale.max.output,
           sigma=sigma,f0=f0,
           sigmethod=sigmethod,
           nrand=nrand)
    }
  }
  
  #now plot
  
}

#' @rdname plotwpmfs
#' @export
plotwpmfs.default<-function(object,...)
{
  stop("Error in plotwpmfs: method not defined for this class")
}