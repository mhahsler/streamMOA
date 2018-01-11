# streamMOA - Interface for MOA Stream Clustering Algorithms - R package

[![CRAN version](http://www.r-pkg.org/badges/version/streamMOA)](https://CRAN.R-project.org/package=streamMOA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/streamMOA)](https://CRAN.R-project.org/package=streamMOA)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/streamMOA.svg?branch=master)](https://travis-ci.org/mhahsler/streamMOA)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/streamMOA?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/streamMOA)

Interface for data stream clustering algorithms implemented in the MOA (Massive Online Analysis) framework. This is an extension package for [stream](http://github.com/mhahsler/stream).

## Installation

* __Stable CRAN version:__ install from within R.
* __Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/streamMOA/build/artifacts) or install via `install_git("mhahsler/streamMOA")` (needs devtools) 

## Example
```R
R> library("streamMOA")
# 3 clusters with 5% noise
R> stream <- DSD_Gaussians(k=3, d=2, noise=.05)
 
# cluster with CluStream  
R> clustream <- DSC_CluStream(m=50, k=3)
R> update(clustream, stream, 500)
R> clustream
CluStream + k-Means (weighted)
Class: DSC_TwoStage, DSC_Macro, DSC 
Number of micro-clusters: 50 
Number of macro-clusters: 3 
 
# plot micro-clusters
R> plot(clustream, stream, type = "both")
```


## Further Information

* [streamMOA package vignette](https://CRAN.R-project.org/package=streamMOA/vignettes/streamMOA.pdf) with complete examples.
* [Reference manual](https://CRAN.R-project.org/package=streamMOA/streamMOA.pdf)

