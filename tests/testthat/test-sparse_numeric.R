test_that("sparse_numeric constructor and validity work", {
  x <- sparse_numeric(
    value  = c(1, 2, 3),
    pos    = c(1L, 3L, 5L),
    length = 5L
  )

  expect_s4_class(x, "sparse_numeric")
  expect_true(validObject(x))

  # underlying length
  expect_equal(length(x), 5L)
})

test_that("mean for sparse_numeric includes zeros", {
  # underlying dense: c(2, 0, 0, 2)
  x <- sparse_numeric(
    value  = c(2, 2),
    pos    = c(1L, 4L),
    length = 4L
  )

  expect_equal(mean(x), 1)
})

test_that("norm computes Euclidean norm correctly", {
  # underlying dense: c(0, 3, 0, 0, 4)
  x <- sparse_numeric(
    value  = c(3, 4),
    pos    = c(2L, 5L),
    length = 5L
  )

  expect_equal(norm(x), 5)
})

test_that("standardize gives mean ~0 and sd ~1", {
  # underlying dense: c(2, 0, 0, -1)
  x <- sparse_numeric(
    value  = c(2, -1),
    pos    = c(1L, 4L),
    length = 4L
  )

  z <- standardize(x)

  # reconstruct dense for checking
  dense <- numeric(z@length)
  if (length(z@pos) > 0L) {
    dense[z@pos] <- z@value
  }

  expect_equal(mean(dense), 0, tolerance = 1e-8)
  expect_equal(sd(dense), 1, tolerance = 1e-8)
})

test_that("standardize fails for degenerate cases", {
  # length <= 1
  x1 <- sparse_numeric(
    value  = numeric(),
    pos    = integer(),
    length = 1L
  )
  expect_error(standardize(x1))

  # all zeros -> variance zero
  x2 <- sparse_numeric(
    value  = numeric(),
    pos    = integer(),
    length = 4L
  )
  expect_error(standardize(x2))
})

test_that("validity catches bad inputs", {
  # mismatched lengths
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(1L),
        length = 3L),
    regexp = "value and pos must have the same length"
  )

  # positions out of range
  expect_error(
    new("sparse_numeric",
        value  = 1,
        pos    = 0L,
        length = 3L),
    regexp = "pos must be between 1 and 'length'"
  )

  # zero values stored
  expect_error(
    new("sparse_numeric",
        value  = c(0, 2),
        pos    = c(1L, 2L),
        length = 3L),
    regexp = "value must not contain zeros"
  )

  # non-increasing positions
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(2L, 2L),
        length = 3L),
    regexp = "pos must be strictly increasing"
  )
})
