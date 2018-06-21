#' For plotting the magnitude of values in \code{tts} objects (and derived objects) 
#' against time and timescale
#' 
#' 
plotmag<-function(object,...)
{
  UseMethod("plotmag",obj)
}


#' @rdname 
#' @export
plotmag.wt<-function()
{
  
}

#' @rdname 
#' @export
plotmag.wmf<-function()
{
  
}

#' @rdname 
#' @export
plotmag.wpmf<-function()
{
  
}

#' @rdname 
#' @export
plotmag.default<-function(object)
{
  stop("Error in plotmag: not defined for this class")
}