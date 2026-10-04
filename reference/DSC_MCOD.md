# Micro-cluster Continuous Outlier Detector (MCOD)

Interface to the MOA implementation of the MCOD algorithm for
distance-based data stream outlier detection.

## Usage

``` r
DSC_MCOD(r = 0.1, t = 50, w = 1000, recheck_outliers = FALSE)

DSOutlier_MCOD(r = 0.1, t = 50, w = 1000, recheck_outliers = TRUE)

get_outlier_positions(x, ...)

recheck_outlier(x, outlier_correlated_id, ...)

clean_outliers(x, ...)
```

## Arguments

- r:

  Radius used to search for neighbors.

- t:

  Minimum number of neighbors required for a point not to be an outlier.

- w:

  Sliding window width in data points.

- recheck_outliers:

  If `TRUE`, allow detected outliers to be checked again.

- x:

  A `DSC_MCOD` object.

- ...:

  Further arguments (currently ignored).

- outlier_correlated_id:

  Identifier of the outlier to check again.

## Value

An object of class `DSC_MCOD` (subclass of
[stream::DSC_Micro](https://rdrr.io/pkg/stream/man/DSC_Micro.html),
[DSC_MOA](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md) and
[stream::DSC](https://rdrr.io/pkg/stream/man/DSC.html)).

## Details

The algorithm detects density-based outliers. An object \\x\\ is defined
to be an outlier if there are less than \\t\\ objects lying at distance
at most \\r\\ from \\x\\.

Outliers are stored and can be retrieved with `get_outlier_positions()`
and checked again with `recheck_outlier()`.

**Note:** The implementation updates the clustering when
[`predict()`](https://rdrr.io/pkg/stream/man/predict.html) is called.

## Functions

- `get_outlier_positions()`: Returns spatial positions of all current
  outliers.

- `recheck_outlier()`: Re-check whether the outlier identified by
  `outlier_correlated_id` is still an outlier. Returns `TRUE` if it is.

- `clean_outliers()`: Forget detected outliers (currently not
  implemented).

## References

Kontaki M, Gounaris A, Papadopoulos AN, Tsichlas K, and Manolopoulos Y
(2016). Efficient and flexible algorithms for monitoring distance-based
outliers over data streams. *Information Systems,* Vol. 55, pp. 37-53.
[doi:10.1109/ICDE.2011.5767923](https://doi.org/10.1109/ICDE.2011.5767923)

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_MOA.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Dalibor Krleža

## Examples

``` r
# Example 1: Clustering with MCOD
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
mcod <- DSC_MCOD(r = .1, t = 3, w = 100)
update(mcod, stream, 100)
mcod
#> Micro-cluster outlier detector 
#> Class: DSC_MCOD, DSC_Micro, DSC_MOA, DSC 
#> Number of micro-clusters: 7 

plot(mcod, stream, n = 100)


# Example 2: Predict outliers (have a class label of NA)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
mcod <- DSOutlier_MCOD(r = .1, t = 3, w = 100)
update(mcod, stream, 100)

plot(mcod, stream, n = 100)


# Retrieve detected outlier positions.
get_outlier_positions(mcod)
#>         X1         X2
#> 1 0.491609 0.04416643

# Example 3: evaluate on a stream
evaluate_static(mcod, stream, n = 100, type = "micro",
  measure = c("crand", "noisePrecision", "outlierjaccard"))
#> Evaluation results for micro-clusters.
#> Points were assigned to micro-clusters.
#> 
#>          cRand noisePrecision outlierJaccard 
#>      0.5138289      1.0000000      1.0000000 
#> attr(,"type")
#> [1] "micro"
#> attr(,"assign")
#> [1] "micro"
```
