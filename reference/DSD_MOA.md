# Base class for MOA-based data stream generators

Abstract base class for MOA-based data stream generators. It inherits
directly from [stream::DSD](https://rdrr.io/pkg/stream/man/DSD.html).

## Usage

``` r
DSD_MOA(...)
```

## Arguments

- ...:

  Further arguments (currently ignored).

## Value

The abstract class cannot be instantiated and produces an error.

## References

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, and Seidl
T (2010). MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. *Journal of Machine Learning Research*,
11, 1601–1604.

## See also

Other DSD_MOA:
[`DSD_RandomRBFGeneratorEvents()`](http://michael.hahsler.net/streamMOA/reference/DSD_RandomRBFGeneratorEvents.md)

## Author

Michael Hahsler

## Examples

``` r
if (FALSE) { # \dontrun{
DSD_MOA()
} # }
```
