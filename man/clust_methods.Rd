% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clust_methods.R
\name{clust_methods}
\alias{clust_methods}
\alias{summary.clust}
\alias{print.clust}
\alias{set_times.clust}
\alias{set_adj.clust}
\alias{set_clusters.clust}
\alias{set_modres.clust}
\alias{set_mns.clust}
\alias{set_dat.clust}
\alias{set_coords.clust}
\alias{set_methodspecs.clust}
\alias{set_wmfs.clust}
\alias{set_wpmfs.clust}
\alias{get_times.clust}
\alias{get_adj.clust}
\alias{get_clusters.clust}
\alias{get_modres.clust}
\alias{get_mns.clust}
\alias{get_dat.clust}
\alias{get_coords.clust}
\alias{get_methodspec.clust}
\alias{get_wmfs.clust}
\alias{get_wpmfs.clust}
\title{Basic methods for the \code{clust} class}
\usage{
\method{summary}{clust}(object, ...)

\method{print}{clust}(x, ...)

\method{set_times}{clust}(obj, newval)

\method{set_adj}{clust}(obj, newval)

\method{set_clusters}{clust}(obj, newval)

\method{set_modres}{clust}(obj, newval)

\method{set_mns}{clust}(obj, newval)

\method{set_dat}{clust}(obj, newval)

\method{set_coords}{clust}(obj, newval)

\method{set_methodspecs}{clust}(obj, newval)

\method{set_wmfs}{clust}(obj, newval)

\method{set_wpmfs}{clust}(obj, newval)

\method{get_times}{clust}(obj)

\method{get_adj}{clust}(obj)

\method{get_clusters}{clust}(obj)

\method{get_modres}{clust}(obj)

\method{get_mns}{clust}(obj)

\method{get_dat}{clust}(obj)

\method{get_coords}{clust}(obj)

\method{get_methodspec}{clust}(obj)

\method{get_wmfs}{clust}(obj)

\method{get_wpmfs}{clust}(obj)
}
\arguments{
\item{object, x, obj}{An object of class \code{clust}}

\item{...}{Not currently used. Included for argument consistency
with existing generics.}

\item{newval}{A new value, for the \code{set_*} methods}
}
\value{
\code{summary.clust} produces a summary of a \code{clust} object.
A \code{print.clust} method is also available. For \code{clust} objects, 
\code{set_*} and \code{get_*} methods are available for all slots (see
the documentation for \code{clust} for a list). The \code{set_*} methods 
just throw an error, to prevent breaking the consistency between the 
slots of a \code{clust} object.
}
\description{
Set, get, summary, and print methods for the \code{clust} class.
}
\examples{
sig<-matrix(.8,5,5)
diag(sig)<-1
lents<-50
if (requireNamespace("mvtnorm",quietly=TRUE))
{
  dat1<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
  dat2<-t(mvtnorm::rmvnorm(lents,mean=rep(0,5),sigma=sig))
}else
{
  dat1<-t(matrix(rep(rnorm(lents),times=5),lents,5))
  dat2<-t(matrix(rep(rnorm(lents),times=5),lents,5))
}
dat<-rbind(dat1,dat2)
times<-1:lents
dat<-cleandat(dat,times,clev=1)$cdat
coords<-data.frame(Y=rep(0,10),X=1:10)
method<-"coh.sig.fast"
h<-clust(dat,times,coords,method,nsurrogs = 50)
#nsurrogs should be much higher for a real application
get_times(h)
summary(h)
print(h)

}
\seealso{
\code{\link{clust}}
}
\author{
Daniel Reuman, \email{reuman@ku.edu}
}
