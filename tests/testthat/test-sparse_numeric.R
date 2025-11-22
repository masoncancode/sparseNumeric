test_that("sparse_numeric new() and validity work", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 3),
           pos    = c(1L, 3L, 5L),
           length = 5L)

  expect_s4_class(x, "sparse_numeric")
  expect_true(validObject(x))
  expect_equal(x@length, 5L)
})

test_that("coercion to and from numeric works", {
  v <- c(0, 1, 0, -2, 0, 3)
  x <- as(v, "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")

  back <- as(x, "numeric")
  expect_equal(back, v)
})

test_that("mean for sparse_numeric includes zeros", {
  # underlying dense: c(2, 0, 0, 2)
  x <- new("sparse_numeric",
           value  = c(2, 2),
           pos    = c(1L, 4L),
           length = 4L)
  expect_equal(mean(x), 1)
})

test_that("norm computes Euclidean norm correctly", {
  # underlying dense: c(0, 3, 0, 0, 4)
  x <- new("sparse_numeric",
           value  = c(3, 4),
           pos    = c(2L, 5L),
           length = 5L)
  expect_equal(norm(x), 5)
})

test_that("standardize gives mean ~0 and sd ~1", {
  # underlying dense: c(2, 0, 0, -1)
  x <- new("sparse_numeric",
           value  = c(2, -1),
           pos    = c(1L, 4L),
           length = 4L)

  z <- standardize(x)

  dense <- numeric(z@length)
  if (length(z@pos)) dense[z@pos] <- z@value

  expect_equal(mean(dense), 0, tolerance = 1e-8)
  expect_equal(sd(dense), 1, tolerance = 1e-8)
})

test_that("standardize errors on degenerate cases", {
  # length <= 1
  x1 <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 1L)
  expect_error(standardize(x1))

  # all zeros -> variance zero
  x2 <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 4L)
  expect_error(standardize(x2))
})

test_that("sparse arithmetic ops behave sensibly", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 4, 0), "sparse_numeric")

  sum_xy <- as(sparse_add(x, y), "numeric")
  diff_xy <- as(sparse_sub(x, y), "numeric")
  prod_xy <- as(sparse_mult(x, y), "numeric")
  dot_xy  <- sparse_crossprod(x, y)

  expect_equal(sum_xy,  c(0, 4, 4, 2))
  expect_equal(diff_xy, c(0, -2, -4, 2))
  expect_equal(prod_xy, c(0, 3, 0, 0))
  expect_equal(dot_xy,  sum(c(0, 1, 0, 2) * c(0, 3, 4, 0)))
})

test_that("validity catches bad inputs", {
  # mismatched lengths
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(1L),
        length = 3L),
    regexp = "value' and 'pos' must have the same length",
    fixed  = FALSE
  )

  # positions out of range
  expect_error(
    new("sparse_numeric",
        value  = 1,
        pos    = 0L,
        length = 3L),
    regexp = "must be in \\[1, length\\]",
    fixed  = FALSE
  )

  # zero values stored
  expect_error(
    new("sparse_numeric",
        value  = c(0, 2),
        pos    = c(1L, 2L),
        length = 3L),
    regexp = "should not store zeros",
    fixed  = FALSE
  )

  # non-increasing positions
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(2L, 2L),
        length = 3L),
    regexp = "strictly increasing",
    fixed  = FALSE
  )
})


test_that("arithmetic operators delegate to sparse_*", {
  a <- as(c(0, 1, 0, 2), "sparse_numeric")
  b <- as(c(0, 3, 4, 0), "sparse_numeric")

  expect_equal(as(a + b, "numeric"), c(0, 4, 4, 2))
  expect_equal(as(a - b, "numeric"), c(0, -2, -4, 2))
  expect_equal(as(a * b, "numeric"), c(0, 3, 0, 0))
})

test_that("plot for sparse_numeric pairs does not error", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(0, 1, 0, 2), "sparse_numeric")
  expect_silent(plot(x, y))
})

test_that("mean of empty sparse_numeric is NA", {
  x <- new("sparse_numeric",
           value  = numeric(),
           pos    = integer(),
           length = 0L)
  expect_true(is.na(mean(x)))
})

test_that("mean handles empty and all-zero sparse_numeric", {
  # truly empty vector
  x_empty <- new("sparse_numeric",
                 value  = numeric(),
                 pos    = integer(),
                 length = 0L)
  expect_true(is.na(mean(x_empty)))

  # all zeros represented sparsely (no stored values)
  x_zero <- new("sparse_numeric",
                value  = numeric(),
                pos    = integer(),
                length = 5L)
  expect_equal(mean(x_zero), 0)
})

test_that("norm of empty and zero sparse_numeric is zero", {
  x_empty <- new("sparse_numeric",
                 value  = numeric(),
                 pos    = integer(),
                 length = 0L)
  expect_equal(norm(x_empty), 0)

  x_zero <- new("sparse_numeric",
                value  = numeric(),
                pos    = integer(),
                length = 5L)
  expect_equal(norm(x_zero), 0)
})

test_that("standardize errors when length <= 1", {
  x1 <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 1L)
  expect_error(standardize(x1), "length <= 1")
})

test_that("standardize errors when variance is zero", {
  # underlying dense: c(1, 1, 1, 1)
  x_const <- new("sparse_numeric",
                 value  = c(1, 1, 1, 1),
                 pos    = c(1L, 2L, 3L, 4L),
                 length = 4L)
  expect_error(standardize(x_const), "Standard deviation is zero")
})

test_that("coercion numeric <-> sparse_numeric round-trips", {
  v <- c(0, 2, 0, -1, 3)
  x <- as(v, "sparse_numeric")
  back <- as(x, "numeric")
  expect_equal(back, v)

  # all zeros vector
  v0 <- c(0, 0, 0, 0)
  x0 <- as(v0, "sparse_numeric")
  expect_s4_class(x0, "sparse_numeric")
  expect_equal(x0@pos, integer())
  expect_equal(x0@value, numeric())
  expect_equal(x0@length, 4L)
  expect_equal(as(x0, "numeric"), v0)
})

test_that("arithmetic operators hit different branch patterns", {
  # different nonzero patterns to exercise while-loop cases
  a <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
  b <- as(c(0, 3, 0, 4, 5), "sparse_numeric")

  # a has nonzeros at 1,3; b at 2,4,5
  sum_ab  <- as(a + b, "numeric")
  diff_ab <- as(a - b, "numeric")
  prod_ab <- as(a * b, "numeric")  # only overlaps at none; should all be 0

  expect_equal(sum_ab,  c(1, 3, 2, 4, 5))
  expect_equal(diff_ab, c(1, -3, 2, -4, -5))
  expect_equal(prod_ab, c(0, 0, 0, 0, 0))

  # dot product via sparse_crossprod
  expect_equal(
    sparse_crossprod(a, b),
    sum(as(a, "numeric") * as(b, "numeric"))
  )
})

test_that("show works for empty and dense-ish sparse_numeric", {
  x_empty <- new("sparse_numeric",
                 value  = numeric(),
                 pos    = integer(),
                 length = 5L)
  expect_output(show(x_empty), "all zeros")

  x_many <- new("sparse_numeric",
                value  = rep(1, 12),
                pos    = as.integer(1:12),
                length = 12L)
  # should print truncated list of positions/values
  expect_output(show(x_many), "sparse_numeric\\(length=12, nnz=12\\)")
})

test_that("plot handles no overlap, overlap, and empty cases", {
  x <- as(c(0, 1, 0, 2, 0), "sparse_numeric")
  y <- as(c(0, 0, 3, 0, 4), "sparse_numeric")
  z <- new("sparse_numeric",
           value  = numeric(),
           pos    = integer(),
           length = 5L)

  # no overlap
  expect_silent(plot(x, y))

  # overlap (shift y so it shares index 2)
  y2 <- as(c(0, 1, 3, 0, 4), "sparse_numeric")
  expect_silent(plot(x, y2))

  # one empty
  expect_silent(plot(x, z))
})





