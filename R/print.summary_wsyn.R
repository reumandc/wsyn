#' Print method for \code{summary.wsyn} class
#' 
#' Print method for \code{summary.wsyn} class
#' 
#' @param x A \code{summary.wsyn} object
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{print.summary_wsyn} is called for its effect of
#' printing to the screen.
#' 
#' @examples 
#' times<-1:10
#' timescales<-1/c(1:10)
#' values<-matrix(1,length(times),length(timescales))
#' h<-tts(times,timescales,values)
#' print(summary(h))
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @export

print.summary_wsyn<-function(x,...)
{
  for (counter in 1:length(x))
  {
    cat(names(x)[counter],": ",x[[counter]],"\n",sep="")
  }
}

