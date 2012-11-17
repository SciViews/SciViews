\name{colors}
\alias{cwm.colors}
\alias{rwb.colors}
\alias{rwg.colors}
\alias{ryg.colors}

\title{Various color palettes}

\description{
  Create vectors of \code{n} contiguous colors.
}

\usage{
cwm.colors(n, alpha = 1, s = 0.9, v = 0.9)
rwb.colors(n, alpha = 1, s = 0.9, v = 0.9)
rwg.colors(n, alpha = 1, s = 0.9, v = 0.9)
ryg.colors(n, alpha = 1, s = 0.9, v = 0.9)
}

\arguments{
  \item{n}{ the number of colors (>= 1) to be in the palette. }
  \item{alpha}{ the alpha transparency, a number in [0, 1], see argument
    \code{alpha} in \code{\link{hsv}}. }
  \item{s}{ the 'saturation' to be used to complete the HSV color descriptions. }
  \item{v}{ the 'value' to use for the HSV color descriptions. }
}

\details{
  \code{cwm.colors(s = 0.5, v = 1)} gives very similar colors to
  \code{cm.colors()}.
  
  \code{ryg.colors()} is similar to \code{rainbow(start = 0, end = 2/6)}.
}

\value{
  A character vector, \code{cv} of color names. This can be used for
  user-defined color palette, using \code{\link{palette}}\code{(cv)}, or a
  \code{col = cv} specification in a graphic function or in \code{\link{par}}.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{cm.colors}}, \code{\link{colorRampPalette}} }

\examples{
## Draw color wheels with various palettes
opar <- par(mfrow = c(2, 2))
pie(rep(1, 11), col = cwm.colors(11), main = "Cyan - white - magenta")
pie(rep(1, 11), col = rwb.colors(11), main = "Red - white - blue")
pie(rep(1, 11), col = rwg.colors(11), main = "Red - white - green")
pie(rep(1, 11), col = ryg.colors(11), main = "Red - yellow - green")
par(opar)
}

\keyword{color}

\concept{ color palettes }
