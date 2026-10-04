# DSC_MOA Class

An abstract class that inherits from the base class
[stream::DSC](https://rdrr.io/pkg/stream/man/DSC.html) and provides the
common functions needed to interface MOA clusterers.

## Usage

``` r
DSC_MOA(...)
```

## Arguments

- ...:

  Further arguments (currently ignored).

## Details

`DSC_MOA` is a subclass of
[stream::DSC](https://rdrr.io/pkg/stream/man/DSC.html) for MOA-based
clusterers. `DSC_MOA` classes operate in a different way in that the
centers of the micro-clusters have to be extracted from the underlying
Java object. This is done by using rJava to perform method calls
directly in the JRI and converting the multi-dimensional Java array into
a local R data type.

**Note:** The formula interface is currently not implemented for
MOA-based clusterers. Use
[stream::DSF](https://rdrr.io/pkg/stream/man/DSF.html) to select
features instead.

## References

Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer (2010).
MOA: Massive Online Analysis, Journal of Machine Learning Research 11:
1601-1604

## See also

Other DSC_MOA:
[`DSC_BICO_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_BICO_MOA.md),
[`DSC_CluStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_CluStream.md),
[`DSC_ClusTree()`](http://michael.hahsler.net/streamMOA/reference/DSC_ClusTree.md),
[`DSC_DStream_MOA()`](http://michael.hahsler.net/streamMOA/reference/DSC_DStream_MOA.md),
[`DSC_DenStream()`](http://michael.hahsler.net/streamMOA/reference/DSC_DenStream.md),
[`DSC_MCOD()`](http://michael.hahsler.net/streamMOA/reference/DSC_MCOD.md),
[`DSC_StreamKM()`](http://michael.hahsler.net/streamMOA/reference/DSC_StreamKM.md)

## Author

Michael Hahsler and John Forrest
