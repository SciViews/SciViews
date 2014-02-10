\name{as.intBase}
\alias{as.integerBase}
\alias{as.intBase}

\title{Convert strings to integers}

\description{
  Convert strings to integers according to the given base using the C function
  \code{strtol}, or choose a suitable base following the C rules.
}

\usage{
as.integerBase(x, base = 0L)
as.intBase(x, base = 0L)
}

\arguments{
  \item{x}{ a character vector, or something coercible to this by \code{as.character}. }
  \item{base}{ an integer which is between 2 and 36 inclusive, or zero (default). }
}

\details{
  TODO...
}

\value{
  TODO...
}

\author{
  This is a wrapper for \code{strtoi()} function in base package.
}

\seealso{ \code{\link[base]{strtoi}} }

\examples{
as.intBase(c("0xff", "077", "123"))
as.intBase(c("ffff", "FFFF"), 16L)
as.intBase(c("177", "377"), 8L)
}

\keyword{character}

\concept{ convert character into integer }
