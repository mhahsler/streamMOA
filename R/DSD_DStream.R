# dstream options:
# -d decayFactor 	0.998 (The decay factor, lambda, in (0,1), range: 0.001 to 0.999)
# -m Cm 	3.0 (Controls the threshold for dense grids, range: 1.001 to Double.MAX_VALUE)
# -l Cl		0.8 (Controls the threshold for sparse grids, in (0,1), range: 0.001 to 0.999)
# -b Beta 0.3 (Adjusts the window of protection for renaming previously deleted grids as sporadic, > 0, range: 0.001 to Double.MAX_VALUE)

DSC_DStream <- function(decayFactor=0.001, Cm=3.0, Cl=0.8, Beta=0.3) {

  ### Java code does parameter checking
  paramList <- list(
    d=decayFactor,
    m=Cm,
    l=Cl,
    b=Beta
  )

  clus <- DSC_MOA_Clusterer("moa/clusterers/dstream/Dstream", "DStream", paramList)

  clus
}
