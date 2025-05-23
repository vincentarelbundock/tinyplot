% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_jitter.R
\name{type_jitter}
\alias{type_jitter}
\title{Jittered points plot type}
\usage{
type_jitter(factor = 1, amount = NULL)
}
\arguments{
\item{factor}{numeric.}

\item{amount}{numeric; if positive, used as \emph{amount} (see below),
    otherwise, if \code{= 0} the default is \code{factor * z/50}.

    Default (\code{NULL}): \code{factor * d/5} where \code{d} is about
    the smallest difference between \code{x} values.}
}
\description{
Type function for plotting jittered points.
Arguments are passed to \code{\link[base]{jitter}}.
}
\details{
The result, say \code{r}, is \code{r <- x + runif(n, -a, a)}
  where \code{n <- length(x)} and \code{a} is the \code{amount}
  argument (if specified).

  Let \code{z <- max(x) - min(x)} (assuming the usual case).
  The amount \code{a} to be added is either provided as \emph{positive}
  argument \code{amount} or otherwise computed from \code{z}, as
  follows:

  If \code{amount == 0}, we set \code{a <- factor * z/50} (same as S).

  If \code{amount} is \code{NULL} (\emph{default}), we set
  \code{a <- factor * d/5} where \emph{d} is the smallest
  difference between adjacent unique (apart from fuzz) \code{x} values.
}
\examples{
# "jitter" type convenience string
tinyplot(Sepal.Length ~ Species, data = iris, type = "jitter")

# Use `type_jitter()` to pass extra arguments for customization
tinyplot(Sepal.Length ~ Species, data = iris, type = type_jitter(factor = 0.5))
}
