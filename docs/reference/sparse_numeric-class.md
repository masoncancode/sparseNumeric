# Sparse numeric vector S4 class

The sparse_numeric class stores a mostly-zero numeric vector in
compressed form using non-zero values and their 1-based positions.

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of 1-based positions of non-zero values.

- `length`:

  Integer giving the length of the underlying full vector.
