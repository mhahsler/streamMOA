
# BICO options:
# -k Cluster 	5 (Number of desired centers, range: 1 to Max_Int)
# -d Dimensions 	10 (Number of the dimensions of the input points, range: 1 to Max_Int)
# -n MaxClusterFeatures 1000 (Maximum size of the coreset, range: 1, Max_Int)
# -p Projections 10 (Number of random projections used for the nearest neighbour search, range: 1 to Max_Int)

DSC_BICO <- function(Cluster=5, Dimensions=10, MaxClusterFeatures=1000, Projections=10) {

  ### Java code does parameter checking
  paramList <- list(
    k=as.integer(Cluster),
    d=as.integer(Dimensions),
    n=as.integer(MaxClusterFeatures),
    p=as.integer(Projections)
  )

  clus <- DSC_MOA_Clusterer("moa/clusterers/kmeanspm/BICO", "BICO", paramList)

  clus
}
