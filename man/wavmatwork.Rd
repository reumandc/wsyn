% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavmatwork.R
\name{wavmatwork}
\alias{wavmatwork}
\title{Facilitates the computations in synmat for coherence and ReXWT methods}
\usage{
wavmatwork(dat, times, scale.min, scale.max.input, sigma, f0, norm, treatment)
}
\arguments{
\item{dat}{A locations (rows) x time (columns) matrix of measurements}

\item{times}{The times at which measurements were made, spacing 1}

\item{scale.min}{The smallest scale of fluctuation that will be examined. At least 2. Used 
only for wavelet-based methods.}

\item{scale.max.input}{The largest scale of fluctuation guaranteed to be examined. Only used 
for wavelet-based methods.}

\item{sigma}{The ratio of each time scale examined relative to the next timescale. Should be 
greater than 1. Only used for wavelet-based methods.}

\item{f0}{The ratio of the period of fluctuation to the width of the envelope. Only used for 
wavelet-based methods.}

\item{norm}{The normalization of wavelet transforms to be used. One of "none", "phase", 
"powind".}

\item{treatment}{Either "Mod" or "Re"}
}
\value{
\code{wavmatwork} returns a list consisting of:
\item{timescales}{The timescales of analysis} 
\item{wavarray}{An array, locations by locations by timescales, containing either the 
coherences (for \code{treatment="Mod"}) or the real parts of the cross-wavelet transforms
(for \code{treatment="Re"}) between locations.}
}
\description{
Worker/utility function serving the analysis carried out in synmat for methods based
on coherence or real part of the cross-wavelet transform.
}
\note{
Internal function, no error checking done.
}
\author{
Daniel Reuman, \email{reuman@ku.edu}
}
