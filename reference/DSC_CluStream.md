# CluStream Data Stream Clusterer

Implements the CluStream algorithm for data streams (Aggarwal et al.,
2003).

## Usage

``` r
DSC_CluStream(m = 100, horizon = 1000, t = 2, k = 5)
```

## Arguments

- m:

  Maximum number of micro-clusters.

- horizon:

  Time window used by CluStream.

- t:

  Maximum boundary factor used to decide whether a new point belongs to
  a micro-cluster. The boundary is `t` times the root mean square
  deviation from the micro-cluster center.

- k:

  Number of macro-clusters produced by weighted k-means.

## Value

An object of class `DSC_CluStream` (subclass of
[stream::DSC_Micro](https://rdrr.io/pkg/stream/man/DSC_Micro.html),
[DSC_MOA](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md) and
[stream::DSC](https://rdrr.io/pkg/stream/man/DSC.html)).

## Details

This is an interface to the MOA implementation of CluStream.

If `k` is specified, then CluStream applies a weighted k-means algorithm
for reclustering (see Examples section below).

## References

Aggarwal CC, Han J, Wang J, Yu PS (2003). "A Framework for Clustering
Evolving Data Streams." In "Proceedings of the International Conference
on Very Large Data Bases (VLDB '03)," pp. 81-92.

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010). MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. In Journal of Machine Learning Research
(JMLR).

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Michael Hahsler and John Forrest

## Examples

``` r
# data with 3 clusters and 5% noise
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)

# cluster with CluStream
clustream <- DSC_CluStream(m = 50, horizon = 100, k = 3)
update(clustream, stream, 500)
clustream
#> CluStream 
#> Class: moa/clusterers/clustream/WithKmeans, DSC_MOA, DSC_Micro, DSC 
#> Number of micro-clusters: 50 
#> Number of macro-clusters: 3 

plot(clustream, stream, type = "both")
```
