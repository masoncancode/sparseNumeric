#' Sparse numeric vector S4 class
#'
#' The \linkS4class{sparse_numeric} class stores a mostly-zero numeric
#' vector in compressed form using non-zero values and their 1-based
#' positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of 1-based positions of non-zero values.
#' @slot length Integer giving the length of the underlying full vector.
#'
#' @export
setClass(
  "sparse_numeric",
  slots = c(
    value  = "numeric",   # non-zero values
    pos    = "integer",   # positions (1-based)
    length = "integer"    # full length of vector
  )
)

# -------------------------------------------------------------------------
# validity
# -------------------------------------------------------------------------

setValidity("sparse_numeric", function(object) {
  err <- character()
  n <- object@length
  v <- object@value
  p <- object@pos

  if (length(n) != 1L || is.na(n) || n < 0L)
    err <- c(err, "'length' must be a single non-negative integer")

  if (length(v) != length(p))
    err <- c(err, "'value' and 'pos' must have the same length")

  if (anyNA(v))           err <- c(err, "'value' cannot contain NA")
  if (any(!is.finite(v))) err <- c(err, "'value' must be finite")
  if (any(v == 0))        err <- c(err, "'value' should not store zeros")

  if (length(p)) {
    if (anyNA(p))            err <- c(err, "'pos' cannot contain NA")
    if (any(p < 1L | p > n)) err <- c(err, "'pos' must be in [1, length]")
    if (any(duplicated(p)))  err <- c(err, "'pos' must be unique")
    if (length(p) > 1L && any(diff(p) <= 0))
      err <- c(err, "'pos' must be strictly increasing")
  }

  if (length(err)) err else TRUE
})

# -------------------------------------------------------------------------
# helpers
# -------------------------------------------------------------------------

make_sparse <- function(val, idx, n) {
  if (length(val)) {
    o <- order(idx)
    idx <- as.integer(idx[o])
    val <- as.numeric(val[o])
    keep <- val != 0
    idx <- idx[keep]; val <- val[keep]
  } else {
    idx <- integer(); val <- numeric()
  }
  new("sparse_numeric", value = val, pos = idx, length = as.integer(n))
}

check_len <- function(x, y) {
  if (x@length != y@length)
    stop("Lengths differ: ", x@length, " vs ", y@length, call. = FALSE)
  invisible(TRUE)
}

add_or_sub <- function(x, y, add = TRUE) {
  xi <- x@pos; xv <- x@value
  yi <- y@pos; yv <- y@value
  i <- j <- 1L
  out_i <- integer(0); out_v <- numeric(0)

  while (i <= length(xi) || j <= length(yi)) {
    if (i <= length(xi) && (j > length(yi) || xi[i] < yi[j])) {
      val <- xv[i]
      if (val != 0) { out_i <- c(out_i, xi[i]); out_v <- c(out_v, val) }
      i <- i + 1L
    } else if (j <= length(yi) && (i > length(xi) || yi[j] < xi[i])) {
      val <- if (add) yv[j] else -yv[j]
      if (val != 0) { out_i <- c(out_i, yi[j]); out_v <- c(out_v, val) }
      j <- j + 1L
    } else { # equal position
      val <- if (add) xv[i] + yv[j] else xv[i] - yv[j]
      if (val != 0) { out_i <- c(out_i, xi[i]); out_v <- c(out_v, val) }
      i <- i + 1L; j <- j + 1L
    }
  }
  make_sparse(out_v, out_i, x@length)
}

mul_intersect <- function(x, y) {
  xi <- x@pos; xv <- x@value
  yi <- y@pos; yv <- y@value
  i <- j <- 1L
  out_i <- integer(0); out_v <- numeric(0)

  while (i <= length(xi) && j <= length(yi)) {
    if (xi[i] == yi[j]) {
      prod <- xv[i] * yv[j]
      if (prod != 0) { out_i <- c(out_i, xi[i]); out_v <- c(out_v, prod) }
      i <- i + 1L; j <- j + 1L
    } else if (xi[i] < yi[j]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  make_sparse(out_v, out_i, x@length)
}

dot_sparse <- function(x, y) {
  xi <- x@pos; xv <- x@value
  yi <- y@pos; yv <- y@value
  i <- j <- 1L; s <- 0
  while (i <= length(xi) && j <= length(yi)) {
    if (xi[i] == yi[j]) { s <- s + xv[i] * yv[j]; i <- i + 1L; j <- j + 1L }
    else if (xi[i] < yi[j]) i <- i + 1L else j <- j + 1L
  }
  s
}

# -------------------------------------------------------------------------
# coercions
# -------------------------------------------------------------------------

setAs("numeric", "sparse_numeric", function(from) {
  idx <- which(from != 0)
  make_sparse(from[idx], as.integer(idx), length(from))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})

# -------------------------------------------------------------------------
# sparse arithmetic generics & methods
# -------------------------------------------------------------------------

#' Sparse arithmetic operations
#'
#' Generics and methods implementing basic arithmetic on
#' \linkS4class{sparse_numeric} vectors.
#'
#' @param x,y \linkS4class{sparse_numeric} objects.
#' @param e1,e2 \linkS4class{sparse_numeric} objects for the arithmetic operators (`+`, `-`, `*`).
#' @param ... Ignored.
#'
#' @return
#' * `sparse_add()`, `sparse_sub()`, and `sparse_mult()` return a
#'   \linkS4class{sparse_numeric} object.
#' * `sparse_crossprod()` returns a numeric scalar (dot product).
#'
#' @name sparse_ops
#' @docType methods
NULL

#' @rdname sparse_ops
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_ops
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_ops
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_ops
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# some sessions don’t have an S4 plot generic yet
if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

# methods for sparse_* ---------------------------------------------------

#' @rdname sparse_ops
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) { check_len(x, y); add_or_sub(x, y, add = TRUE) })

#' @rdname sparse_ops
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) { check_len(x, y); add_or_sub(x, y, add = FALSE) })

#' @rdname sparse_ops
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) { check_len(x, y); mul_intersect(x, y) })

#' @rdname sparse_ops
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) { check_len(x, y); dot_sparse(x, y) })

# -------------------------------------------------------------------------
# operators
# -------------------------------------------------------------------------

#' @rdname sparse_ops
setMethod("+", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_ops
setMethod("-", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_ops
setMethod("*", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# -------------------------------------------------------------------------
# show & plot
# -------------------------------------------------------------------------

setMethod("show", "sparse_numeric", function(object) {
  k <- length(object@pos)
  cat(sprintf("sparse_numeric(length=%d, nnz=%d)\n", object@length, k))
  if (!k) {
    cat("  (all zeros)\n")
  } else {
    n <- min(10L, k)
    cat("  pos : ", paste(object@pos[seq_len(n)], collapse = " "),
        if (k > n) " ..." else "", "\n", sep = "")
    cat("  vals: ", paste(signif(object@value[seq_len(n)], 6), collapse = " "),
        if (k > n) " ..." else "", "\n", sep = "")
  }
})

setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            check_len(x, y)
            if (!length(x@pos) || !length(y@pos)) {
              plot(NA, xlim = c(0.5, max(1L, x@length)), ylim = c(0, 1), xaxt = "n",
                   xlab = "Index", ylab = "Value", main = "Overlapping non-zero elements")
              legend("topright", "overlap: 0", bty = "n")
              return(invisible())
            }
            m <- match(x@pos, y@pos, nomatch = 0L)
            idx <- which(m > 0L)
            if (!length(idx)) {
              plot(NA, xlim = c(0.5, max(1L, x@length)), ylim = c(0, 1), xaxt = "n",
                   xlab = "Index", ylab = "Value", main = "Overlapping non-zero elements")
              legend("topright", "overlap: 0", bty = "n")
              return(invisible())
            }
            xv <- x@value[idx]
            yv <- y@value[m[idx]]
            plot(xv, yv,
                 xlab = "x (overlap)", ylab = "y (overlap)",
                 main = sprintf("Overlap: %d", length(idx)), ...)
            abline(h = 0, v = 0, lty = 3)
          })

# -------------------------------------------------------------------------
# mean()
# -------------------------------------------------------------------------

#' Mean of a sparse_numeric vector
#'
#' Computes the mean of the underlying full vector (including implicit
#' zeros) without coercing to a dense representation.
#'
#' @param x A \linkS4class{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return Numeric scalar mean.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  n <- as.integer(x@length)
  if (n == 0L) {
    return(NA_real_)
  }
  sum(x@value) / n
})

# -------------------------------------------------------------------------
# norm()
# -------------------------------------------------------------------------

#' Compute a vector norm
#'
#' Generic for computing norms of objects. For a
#' \linkS4class{sparse_numeric} vector this returns the Euclidean (L2)
#' norm \eqn{sqrt(sum(x_i^2))}.
#'
#' @param x Object to compute the norm of.
#' @param ... Ignored.
#'
#' @return A numeric scalar.
#' @export
#' @aliases norm norm,sparse_numeric-method
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

# -------------------------------------------------------------------------
# standardize()
# -------------------------------------------------------------------------

#' Standardize an object
#'
#' Generic to create a standardized version of an object. For
#' \linkS4class{sparse_numeric} vectors, this centers and scales the
#' underlying dense vector (including zeros) so that it has mean 0 and
#' sample standard deviation 1.
#'
#' @param x Object to standardize.
#' @param ... Ignored.
#'
#' @return A new \linkS4class{sparse_numeric} object for sparse inputs.
#' @export
#' @aliases standardize standardize,sparse_numeric-method
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- as.integer(x@length)
  if (n <= 1L) {
    stop("Cannot standardize a vector of length <= 1.")
  }

  sum_x  <- sum(x@value)
  sum_x2 <- sum(x@value^2)

  mu <- sum_x / n

  var_num <- sum_x2 - n * mu^2
  var_den <- n - 1L
  if (var_den <= 0L) {
    stop("Cannot standardize: not enough observations.")
  }
  var <- var_num / var_den
  if (!is.finite(var) || var <= 0) {
    stop("Standard deviation is zero or not finite; cannot standardize.")
  }
  s <- sqrt(var)

  dense <- numeric(n)
  if (length(x@pos)) dense[x@pos] <- x@value
  dense_std <- (dense - mu) / s

  nz <- which(dense_std != 0)
  if (!length(nz)) {
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = as.integer(n)))
  }

  new("sparse_numeric",
      value  = as.numeric(dense_std[nz]),
      pos    = as.integer(nz),
      length = as.integer(n))
})

