# wsyn: wavelet approaches to synchrony

Lawrence Sheppard, University of Kansas  
Jonathan Walter, University of Virginia  
Thomas Anderson, University of Kansas  
Lei Zhao, China Agricultural University  
Daniel C. Reuman, University of Kansas - maintainer 

This repository provides tools for analyzing synchrony of time series from a timescale-specific viewpoint. The tools have so far principally been useful for studying spatial synchrony of population dynamics in ecology, but may be useful for a variety of other applications.

## Explanations

In addition to the documentation, see the vignette, which is the best way to get started.

## Installation

For a quick install of this development version: devtools::install_github(repo="reumandc/wsyn"). The 
install_github function, by default, will not install the vignette. There is 
an option for installing vignettes (build_vignettes=TRUE), but it seems 
finicky. You can also clone the repository and use devtools::install() 
to install from your local clone. The build_vignettes=TRUE option seems 
to work better for devtools::install than it does 
for devtools::install_github. If you have trouble, please email me.

## Running all tests

Some of the more intensive tests are skipped unless there is a variable in the
global environment called RUN_INTENSIVE_TESTS which is TRUE. This is so CRAN checks
will run in the allowed time on CRAN servers, and was requested by CRAN.
