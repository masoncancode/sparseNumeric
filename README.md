# sparseNumeric


`sparseNumeric` provides an S4 sparse numeric vector class, `sparse_numeric`,
for storing mostly-zero numeric vectors. The package includes methods for:

- Computing means without materializing dense vectors.
- Computing Euclidean norms.
- Standardizing vectors (center and scale) for modeling.

## Installation

You can install the development version from GitHub once the repository
is online:

```r
# install.packages("remotes")
remotes::install_github("<USER>/<REPO>")
