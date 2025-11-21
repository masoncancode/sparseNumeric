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

