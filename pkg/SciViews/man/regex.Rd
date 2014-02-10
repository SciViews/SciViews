\name{regex}
\alias{regex}
\alias{is.regex}
\alias{print.regex}
\alias{pcre}
\alias{is.pcre}
\alias{print.pcre}

\title{Regular expressions}

\description{
  Create regular expression (either 'regex' -POSIX 1003.2 extended regular
  expressions-  or 'pcre' -perl compatibe regular expressions- objects).
}

\usage{
## Create, test or print a regular expression object
regex(pattern, use.bytes = FALSE, globbing, trim.head = FALSE, trim.tail = TRUE)
is.regex(x)
\method{print}{regex}(x, \dots)

## Create, test or print a perl-compatible regular expression object
pcre(pattern, use.bytes = FALSE)
is.pcre(x)
\method{print}{pcre}(x, \dots)
}

\arguments{
  \item{pattern}{ character string with the pattern to use for the regular expression. }
  \item{use.bytes}{ logical. If \code{TRUE}, the matching is done byte-by-byte
    rather than character-by-character. }
  \item{globbing}{ character string with wildcard or globbing pattern to be
    transformed into a regular expression. If provided, \code{pattern} is ignored. }
  \item{trim.head}{ associated to \code{globbing} only. Specify if \code{"^.*"}
    should be trimmed from the result. }
  \item{trim.tail}{ associated to \code{globbing} only. Specify if \code{".*$"}
    should be trimmed from the result. }
  \item{x}{ an object. }  
  \item{\dots}{ unused. }
}

\details{
  TODO...
}

\value{
  TODO...
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but these are indeed wrappers
  around existing functions written by the R Core Team in base or utils packages.
}

\seealso{ \code{\link{char}}, \code{\link[base]{character}} }

\examples{
## TODO: various examples of character string manipulations 
}

\keyword{character}

\concept{ regular expressions for character strings manipulation }
