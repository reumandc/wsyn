#' Print method for \code{summary_wsyn} class
#' 
#' Print method for \code{summary_wsyn} class
#' 
#' @param x A \code{summary_wsyn} object
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{print.summary_wsyn} is called for its effect of
#' printing to the screen.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{tts_methods}}, \code{\link{wt_methods}}, \code{\link{wmf_methods}}, \code{\link{wpmf_methods}}, 
#' \code{\link{coh_methods}}, \code{\link{wlm_methods}}, \code{\link{wlmtest_methods}}, \code{\link{clust_methods}},
#' \code{browseVignettes("wsyn")}
#' 
#' @examples 
#' times<-1:10
#' timescales<-1/c(1:10)
#' values<-matrix(1,length(times),length(timescales))
#' h<-tts(times,timescales,values)
#' print(summary(h))
#' 
#' @export

print.summary_wsyn<-function(x,...)
{
  for (counter in 1:length(x))
  {
    cat(names(x)[counter],": ",x[[counter]],"\n",sep="")
  }
}

