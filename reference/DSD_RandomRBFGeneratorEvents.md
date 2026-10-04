# Random RBF Generator Events Data Stream Generator

Generates random data using MOA's `RandomRBFGeneratorEvents` stream.

## Usage

``` r
DSD_RandomRBFGeneratorEvents(
  k = 3,
  d = 2,
  numClusterRange = 3L,
  kernelRadius = 0.07,
  kernelRadiusRange = 0,
  densityRange = 0,
  speed = 100L,
  speedRange = 0L,
  noiseLevel = 0.1,
  noiseInCluster = FALSE,
  eventFrequency = 30000L,
  eventMergeSplitOption = FALSE,
  eventDeleteCreate = FALSE,
  modelSeed = NULL,
  instanceSeed = NULL
)
```

## Arguments

- k:

  Average number of centroids in the model.

- d:

  Number of dimensions in the generated stream.

- numClusterRange:

  Range for the number of clusters.

- kernelRadius:

  Average radius of the micro-clusters.

- kernelRadiusRange:

  Range of variation in micro-cluster radii.

- densityRange:

  Range of variation in cluster density.

- speed:

  Number of points between kernel movements.

- speedRange:

  Range of variation in kernel speed.

- noiseLevel:

  Proportion of noise points.

- noiseInCluster:

  If `TRUE`, allow noise points inside clusters.

- eventFrequency:

  Number of points between concept-drift events.

- eventMergeSplitOption:

  If `TRUE`, enable cluster merge and split events.

- eventDeleteCreate:

  If `TRUE`, enable cluster deletion and creation events.

- modelSeed:

  Random seed for the cluster model.

- instanceSeed:

  Random seed for generated instances.

## Value

An object of class `DSD_RandomRBFGeneratorEvents` (subclass of
[DSD_MOA](http://michael.hahsler.net/streamMOA/reference/DSD_MOA.md),
[stream::DSD](https://rdrr.io/pkg/stream/man/DSD.html)).

## Details

Only a subset of the parameters supported by the underlying MOA
generator is exposed. If `modelSeed` or `instanceSeed` is `NULL`, a seed
is sampled from R's random-number generator. Set these arguments
explicitly to reproduce a stream; call
[`set.seed()`](https://rdrr.io/r/base/Random.html) to make the generated
default seeds reproducible.

By default, the generator creates three clusters with concept drift.
Cluster locations move over time, and clusters may merge.

## References

Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen, Hardy
Kremer, Timm Jansen, Thomas Seidl. MOA: Massive Online Analysis, a
Framework for Stream Classification and Clustering *Journal of Machine
Learning Research (JMLR)*, 2010.

## See also

Other DSD_MOA:
[`DSD_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSD_MOA.md)

## Author

Michael Hahsler and John Forrest

## Examples

``` r
stream <- DSD_RandomRBFGeneratorEvents()
get_points(stream, 10)
#>           X1        X2 .class
#> 1  0.4618077 0.1416636      2
#> 2  0.5278374 0.5637230      3
#> 3  0.5966436 0.1254461     NA
#> 4  0.4769715 0.5308331      3
#> 5  0.4736576 0.1626657      2
#> 6  0.4855850 0.2288092      2
#> 7  0.5218328 0.5648703      3
#> 8  0.3118304 0.9568799      1
#> 9  0.4443428 0.1496668      2
#> 10 0.1950602 0.9048127      1

if (interactive()) {
animate_data(stream, n = 5000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
}
```
