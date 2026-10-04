# streamKM++

This is an interface to the MOA implementation of streamKM++.

## Usage

``` r
DSC_StreamKM(sizeCoreset = 10000, numClusters = 5, length = 100000L, ...)
```

## Arguments

- sizeCoreset:

  Size of the coreset.

- numClusters:

  Number of clusters to compute.

- length:

  Maximum number of points in the data stream.

- ...:

  Further arguments (currently ignored).

## Details

streamKM++ uses a tree-based sampling strategy to build a small weighted
sample of the stream (a coreset). The MOA implementation applies
k-means++ to find the requested number of centers in the coreset.

**Notes**

- The clusterer can process at most `length` points. Processing more
  points causes an `ArrayIndexOutOfBoundsException`.

- The coreset is not exposed as micro-clusters; only macro-clusters can
  be requested.

## References

Marcel R. Ackermann, Christiane Lammersen, Marcus Maertens, Christoph
Raupach, Christian Sohler, Kamil Swierkot. StreamKM++: A Clustering
Algorithm for Data Streams. In: *Proceedings of the 12th Workshop on
Algorithm Engineering and Experiments (ALENEX '10)*, 2010.

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md)

## Author

Matthias Carnein

## Examples

``` r
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# cluster with streamKM++
streamkm <- DSC_StreamKM(sizeCoreset = 100, numClusters = 3, length = 1000)
update(streamkm, stream, 100)
streamkm
#> StreamKM 
#> Class: moa/clusterers/streamkm/StreamKM, DSC_MOA, DSC_Micro, DSC 
#> Number of macro-clusters: 3 

# plot macro-clusters (no access to micro-clusters)
plot(streamkm, stream)
```
