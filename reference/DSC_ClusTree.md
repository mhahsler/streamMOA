# ClusTree Data Stream Clusterer

Interface to the MOA implementation of the ClusTree data stream
clustering algorithm (Kranen et al., 2009).

## Usage

``` r
DSC_ClusTree(horizon = 1000, maxHeight = 8, lambda = NULL, k = NULL)
```

## Arguments

- horizon:

  Length of the time window.

- maxHeight:

  Maximum height of the tree.

- lambda:

  Value used to override the computed decay parameter.

- k:

  If specified, use k-means with `k` clusters for reclustering.

## Value

An object of class `DSC_ClusTree` (subclass of
[stream::DSC](https://rdrr.io/pkg/stream/man/DSC.html),
[DSC_MOA](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[stream::DSC_Micro](https://rdrr.io/pkg/stream/man/DSC_Micro.html)).

## Details

ClusTree uses a compact, self-adaptive index structure to maintain
stream summaries. Kranen et al. (2009) suggest EM or k-means for
reclustering.

## References

Philipp Kranen, Ira Assent, Corinna Baldauf, and Thomas Seidl. 2009.
Self-Adaptive Anytime Stream Clustering. In Proceedings of the 2009
Ninth IEEE International Conference on Data Mining (ICDM '09). IEEE
Computer Society, Washington, DC, USA, 249-258.
[doi:10.1109/ICDM.2009.47](https://doi.org/10.1109/ICDM.2009.47)

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010). MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. In Journal of Machine Learning Research
(JMLR).

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Michael Hahsler and John Forrest

## Examples

``` r
# data with 3 clusters
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

clustree <- DSC_ClusTree(maxHeight = 3)
update(clustree, stream, 500)
clustree
#> ClusTree 
#> Class: moa/clusterers/clustree/ClusTree, DSC_MOA, DSC_Micro, DSC 
#> Number of micro-clusters: 27 

plot(clustree, stream)


# Use the k-means reclusterer with k = 3 to create macro-clusters
clustree <- DSC_ClusTree(maxHeight = 3, k = 3)
update(clustree, stream, 500)
clustree
#> ClusTree + k-Means (weighted) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 29 
#> Number of macro-clusters: 3 

plot(clustree, stream, type = "both")
```
