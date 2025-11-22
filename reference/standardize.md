# Standardize an object

Generic to create a standardized version of an object. For
[sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
vectors, this centers and scales the underlying dense vector (including
zeros) so that it has mean 0 and sample standard deviation 1.

## Usage

``` r
standardize(x, ...)

# S4 method for class 'sparse_numeric'
standardize(x, ...)
```

## Arguments

- x:

  Object to standardize.

- ...:

  Ignored.

## Value

A new
[sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
object for sparse inputs.
