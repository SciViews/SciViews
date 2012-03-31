\name{ln}
\alias{ln}
\alias{ln1p}
\alias{lg}
\alias{lg1p}
\alias{e}
\alias{lb}

\title{Logarithms}

\description{
  To avoid confusion using the default \code{log()} function, which is natural
  logarithm, but spells out like base 10 logarithm in the mind of some beginneRs,
  we define \code{ln()} and \code{ln1p()} as wrappers for \code{log()} with
  default \code{base = exp(1)} argument and for \code{log1p()}, respectively.
  For similar reasons, \code{lg()} is a wrapper of \code{log10()} (there is no
  possible confusion here, but 'lg' is another common notation for base 10
  logarithm). \code{lg1p()} is a convenient way to use the optimized code to
  calculate the logarithm of x + 1, but returning the result in base 10
  logarithm. \code{e} is the euler constant and is provided for convenience as
  \code{exp(1)}. Finally \code{lb()} is a synonym of \code{log2()}.
}

\usage{
ln(x)
ln1p(x)
lg(x)
lg1p(x)
e
lb(x)
}

\arguments{
  \item{x}{ a numeric or complex vector. }
}

\value{
  A vector of the same length as \code{x} containing the transformed values.
  \code{ln(0)} gives \code{-Inf}, and negative values give \code{NaN}.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but these are just convenient
  wrappers around standard R logarithm functions in the base package.
}

\seealso{ \code{\link{log}} }

\examples{
ln(exp(3))              # Same as log(exp(3))
ln1p(c(0, 1, 10, 100))  # Wrapper for log1p()
lg(10^3)                # Same as log10(10^3)
lg1p(c(0, 1, 10, 100))  # log10(x + 1), but optimized for x << 1
E^4                     # Similar to exp(4), but different calculation!
## Note: exp(4) is to be preferred to E^4, if possible!
lb(1:3)                 # Wrapper for log2()
}

\keyword{ math }

\concept{ logarithm and exponential }
