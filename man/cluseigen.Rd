% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluseigen.R
\name{cluseigen}
\alias{cluseigen}
\title{Community structure detection in networks}
\usage{
cluseigen(adj)
}
\arguments{
\item{adj}{An adjacency matrix. Should be symmetric with diagonal containing zeros.}
}
\value{
\code{cluseigen} returns a list with one element for each of the splits 
performed by the clustering algorithm. Each element is a vector with entries 
corresponding to rows and columns of adj and indicating the module membership
of the node, following the split. The last element of the list is the final 
clustering determined by the algorithm when its halting condition is satisfied.
The first element is always a vector of all 1s (corresponding to before any 
splits are performed).
}
\description{
Community structure detection in networks based on the leading eigenvector of the 
community matrix
}
\details{
The difference between this function and the algorithm described
by Newman is that this function can be used on an adjacency matrix with 
negative elements, which is very common for correlation matrices and other 
measures of pairwise synchrony of time series.
}
\examples{
adj<-matrix(0, 10, 10) # create a fake adjacency matrix
adj[lower.tri(adj)]<-runif(10*9/2, -1, 1)
adj<-adj+t(adj)
colnames(adj)<-letters[1:10]
z<-cluseigen(adj)

}
\references{
Gomez S., Jensen P. & Arenas A. (2009). Analysis of community structure 
in networks of correlated data. Phys Rev E, 80, 016114. 

Newman M.E.J. (2006). Finding community structure in networks using the eigenvectors of 
matrices. Phys Rev E, 74, 036104.

Newman M.E.J. (2006) Modularity and community structure in networks. PNAS 103, 8577-8582.
}
\seealso{
\code{\link{clust}}, \code{\link{modularity}}, \code{browseVignettes("wsyn")}
}
\author{
Lei Zhao, \email{lei.zhao@cau.edu.cn}; Daniel Reuman, \email{reuman@ku.edu}
}
