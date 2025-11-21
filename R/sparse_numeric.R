#' Sparse numeric vector S4 class
#'
#' `sparse_numeric` stores a mostly-zero numeric vector in compressed form.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of 1-based positions of the non-zero values.
#' @slot length Length of the full underlying vector.
#'
#' @export
setClass(
  "sparse_numeric",
  slots = list(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  ),
  validity = function(object) {
    # value and pos must have the same length
    if (length(object@value) != length(object@pos)) {
      return("value and pos must have the same length")
    }

    # positions must be within 1:length
    if (any(object@pos < 1L) || any(object@pos > object@length)) {
      return("pos must be between 1 and 'length'")
    }

    # no stored zeros
    if (any(object@value == 0)) {
      return("value must not contain zeros")
    }

    # positions must be strictly increasing
    if (any(diff(object@pos) <= 0)) {
      return("pos must be strictly increasing")
    }

    TRUE
  }
)

#' Construct a sparse_numeric vector
#'
#' @param value Numeric vector of non-zero values.
#' @param pos Integer vector of 1-based positions of non-zero values.
#' @param length Integer, length of the full underlying vector. If omitted
#'   and `pos` is non-empty, the length defaults to `max(pos)`.
#'
#' @return An object of class [`sparse_numeric`].
#' @export
sparse_numeric <- function(value = numeric(),
                           pos = integer(),
                           length = if (length(pos) == 0L) 0L else max(pos)) {
  new(
    "sparse_numeric",
    value  = as.numeric(value),
    pos    = as.integer(pos),
    length = as.integer(length)
  )
}

# helper: internal dense reconstruction
.to_dense <- function(x) {
  v <- numeric(x@length)
  if (length(x@pos) > 0L) {
    v[x@pos] <- x@value
  }
  v
}

#' @export
setMethod(
  "length",
  signature(x = "sparse_numeric"),
  function(x) {
    x@length
  }
)

#' @export
setMethod(
  "show",
  signature(object = "sparse_numeric"),
  function(object) {
    cat("An object of class \"sparse_numeric\"\n")
    cat("Length:", object@length, "\n")
    cat("Non-zero entries:", length(object@value), "\n")
    if (length(object@value) > 0L) {
      df <- data.frame(
        pos   = object@pos,
        value = object@value
      )
      print(df, row.names = FALSE)
    }
  }
)

#-----------------------------
# mean() for sparse_numeric
#-----------------------------

#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a sparse vector, including all implicit zeros,
#' without coercing to a dense numeric vector.
#'
#' @param x A [`sparse_numeric`] object.
#' @param ... Ignored.
#'
#' @return Numeric scalar, the mean of the full underlying vector.
#' @export
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    n <- as.integer(x@length)
    if (n == 0L) {
      return(NA_real_)
    }
    sum(x@value) / n
  }
)

#-----------------------------
# norm() generic + method
#-----------------------------

#' Norm of a vector
#'
#' For [`sparse_numeric`] objects this returns the Euclidean (L2) norm.
#' For other objects it falls back to [base::norm()].
#'
#' @param x Object for which to compute a norm.
#' @param ... Passed on to methods.
#'
#' @export
setGeneric(
  "norm",
  function(x, ...) standardGeneric("norm"),
  useAsDefault = function(x, ...) base::norm(x, ...)
)

#' @describeIn norm Euclidean norm of a sparse_numeric vector.
#'
#' @export
setMethod(
  "norm",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    sqrt(sum(x@value^2))
  }
)

#-----------------------------
# standardize() generic + method
#-----------------------------

#' Standardize a vector
#'
#' Generic for creating a standardized version of an object.
#'
#' @param x Object to standardize.
#' @param ... Passed on to methods.
#'
#' @export
setGeneric(
  "standardize",
  function(x, ...) standardGeneric("standardize")
)

#' Standardize a sparse_numeric vector
#'
#' Centers and scales a [`sparse_numeric`] vector so that the underlying
#' dense vector (including zeros) has mean 0 and standard deviation 1.
#'
#' @param x A [`sparse_numeric`] object.
#' @return A new [`sparse_numeric`] object containing standardized values.
#' @export
setMethod(
  "standardize",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    n <- as.integer(x@length)
    if (n <= 1L) {
      stop("Cannot standardize a vector of length <= 1.")
    }

    sum_x  <- sum(x@value)
    sum_x2 <- sum(x@value^2)

    mu <- sum_x / n
    # sample variance (like stats::var)
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

    # build dense, standardize, then convert back to sparse
    full <- .to_dense(x)
    full_std <- (full - mu) / s

    nz_pos <- which(full_std != 0)
    if (length(nz_pos) == 0L) {
      return(new(
        "sparse_numeric",
        value  = numeric(),
        pos    = integer(),
        length = as.integer(n)
      ))
    }

    new(
      "sparse_numeric",
      value  = full_std[nz_pos],
      pos    = as.integer(nz_pos),
      length = as.integer(n)
    )
  }
)
