% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errcheck_tsdat.R
\name{errcheck_tsdat}
\alias{errcheck_tsdat}
\title{Error check for appropriate temporal data}
\usage{
errcheck_tsdat(times, t.series, callfunc)
}
\arguments{
\item{times}{times of measurement, spacing 1}

\item{t.series}{the measurements}

\item{callfunc}{the function from which this one was called, for error tracking}
}
\value{
\code{errcheck_tsdat} returns nothing but throws and error if inputs not appropriate
}
\description{
Error checking whether a times vector and t.series vector make a 
legitimate time series for wavelet analysis
}
\author{
Daniel Reuman, \email{reuman@ku.edu}
}
