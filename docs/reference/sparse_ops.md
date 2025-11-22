# Sparse arithmetic operations

Generics and methods implementing basic arithmetic on
[sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
vectors.

## Usage

``` r
sparse_add(x, y, ...)

sparse_sub(x, y, ...)

sparse_mult(x, y, ...)

sparse_crossprod(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_add(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_sub(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_mult(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_crossprod(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 + e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 - e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 * e2
```

## Arguments

- x, y:

  [sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
  objects.

- ...:

  Ignored.

- e1, e2:

  [sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
  objects for the arithmetic operators (`+`, `-`, `*`).

## Value

- `sparse_add()`, `sparse_sub()`, and `sparse_mult()` return a
  [sparse_numeric](https://masoncancode.github.io/sparseNumeric/reference/sparse_numeric-class.md)
  object.

- `sparse_crossprod()` returns a numeric scalar (dot product).
