# D-Stream Data Stream Clustering Algorithm

This is an interface to the MOA implementation of D-Stream. A C++
implementation (including reclustering with attraction) is available as
[stream::DSC_DStream](https://rdrr.io/pkg/stream/man/DSC_DStream.html).

## Usage

``` r
DSC_DStream_MOA(decayFactor = 0.998, Cm = 3, Cl = 0.8, Beta = 0.3)
```

## Arguments

- decayFactor:

  Decay factor applied to grid-cell density.

- Cm:

  Threshold for classifying grid cells as dense.

- Cl:

  Threshold for classifying grid cells as sparse.

- Beta:

  Adjusts the window of protection for renaming previously deleted grids
  as sporadic.

## Details

D-Stream creates an equally spaced grid and estimates the density in
each grid cell using the count of points falling in the cells. Grid
cells are classified based on density into dense, transitional and
sporadic cells. The density is faded after every new point by a decay
factor.

**Notes**

- The implementation uses a 1-by-1 grid, so the example expands the data
  range.

- The MOA implementation does not currently return micro-clusters.

## References

Yixin Chen and Li Tu. 2007. Density-based clustering for real-time
stream data. In Proceedings of the 13th ACM SIGKDD International
Conference on Knowledge Discovery and Data Mining (KDD '07). ACM, New
York, NY, USA, 133-142.

Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density
and attraction. ACM Transactions on Knowledge Discovery from Data, 3(3),
Article 12 (July 2009), 27 pages.

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Matthias Carnein

## Examples

``` r
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05, space_limit = c(0, 10))

# cluster with D-Stream
dstream <- DSC_DStream_MOA(Cm = 3)
update(dstream, stream, 1000)
dstream
#> DStream 
#> Class: moa/clusterers/dstream/Dstream, DSC_MOA, DSC_Micro, DSC 
#> Number of macro-clusters: 3 

# plot macro-clusters
plot(dstream, stream, type = "macro")
```
