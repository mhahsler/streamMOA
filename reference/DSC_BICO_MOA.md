# BICO: Fast computation of k-means coresets in a data stream

This is an interface to the MOA implementation of BICO. The original
BICO implementation by Fichtenberger et al is also available as
[stream::DSC_BICO](https://rdrr.io/pkg/stream/man/DSC_BICO.html).

## Usage

``` r
DSC_BICO_MOA(
  Cluster = 5,
  Dimensions,
  MaxClusterFeatures = 1000,
  Projections = 10,
  k = NULL,
  space = NULL,
  p = NULL
)
```

## Arguments

- Cluster:

  Number of desired centers.

- Dimensions:

  Number of dimensions in the input stream; this must be specified in
  advance.

- MaxClusterFeatures:

  Maximum number of cluster features in the coreset.

- Projections:

  Number of random projections used for the nearest neighbor search.

- k:

  Alias for `Cluster`.

- space:

  Alias for `MaxClusterFeatures`.

- p:

  Alias for `Projections`.

## Details

BICO maintains a tree which is inspired by the clustering tree of BIRCH,
a SIGMOD Test of Time award-winning clustering algorithm. Each node in
the tree represents a subset of these points. Instead of storing all
points as individual objects, only the number of points, the sum and the
squared sum of the subset's points are stored as key features of each
subset. Points are inserted into exactly one node.

## References

Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris
Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means
Clustering. ESA 2013: 481-492

## See also

Other DSC_MOA:
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Matthias Carnein

## Examples

``` r
# data with 3 clusters and 2 dimensions
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# cluster with BICO
bico <- DSC_BICO_MOA(Cluster = 3, Dimensions = 2)
update(bico, stream, 100)
bico
#> BICO 
#> Class: moa/clusterers/kmeanspm/BICO, DSC_MOA, DSC_Micro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 3 

# plot micro and macro-clusters
plot(bico, stream, type = "both")
```
