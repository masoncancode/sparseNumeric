# Compute a vector norm

Generic for computing norms of objects. For a
[sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
vector this returns the Euclidean (L2) norm \\sqrt(sum(x_i^2))\\.

## Usage

``` r
norm(x, ...)

# S4 method for class 'sparse_numeric'
norm(x, ...)
```

## Arguments

- x:

  Object to compute the norm of.

- ...:

  Ignored.

## Value

A numeric scalar.
