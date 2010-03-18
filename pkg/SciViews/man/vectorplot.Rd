\name{vectorplot}
\Rdversion{1.1}
\alias{vectorplot}
\alias{vectorplot.default}
\alias{vectorplot.loadings}
\alias{vectorplot.correlation}

\title{Plot vectors inside a unit circle (PCA loadings or correlations plots)}

\description{
  Plots vectors with 0<norms<1 inside a circle. These plots are mainly designed
  to represent variables in principal components space for PCAs.
}

\usage{
vectorplot(x, \dots)
\method{vectorplot}{default}(x, y, col = par("col"), circle.col = "gray",
    ar.length = 0.1, pos = NULL, cex = par("cex"), labels = NULL, \dots)
\method{vectorplot}{loadings}(x, choices = 1L:2L, col = par("col"),
    circle.col = "gray", ar.length = 0.1, pos = NULL, cex = par("cex"),
    labels = rownames(x), main = deparse(substitute(x)), \dots)
\method{vectorplot}{correlation}(x, choices = 1L:2L, col = par("col"),
    circle.col = "gray", ar.length = 0.1, pos = NULL, cex = par("cex"),
    labels = rownames(x), main = deparse(substitute(x)), \dots)
}

\arguments{
  \item{x}{ an object that has a \code{vectorplot()} method, like 'loadings'
    or 'correlation', or a numeric vector with 0<values<1. }
  \item{y}{ a numeric vector with 0<values<1 of same length as \code{x}. }
  \item{choices}{ a vector of two integers indicating the axes to plot. }
  \item{col}{ color of the arrows and labels. }
  \item{circle.col}{ the color for the circle around the vector plot. }
  \item{ar.length}{ the length of the arrows. }
  \item{pos}{ the position of text relative to arrows. If \code{NULL}, a
    suitable position is calculated according to the direction where the
	arrows are pointing. }
  \item{cex}{ the factor of expansion for labels in the graph. }
  \item{labels}{ the labels to write. }
  \item{main}{ the title of the graph. }
  \item{\dots}{ further arguments passed to plot functions. }
}

\value{
  The object 'x' is returned invisibly. These functions are called for their
  side-effect of drawing a vector plot.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>.
}

\seealso{ \code{\link{pcomp}}, \code{\link{loadings}},
  \code{\link{correlation}} }

\examples{
# Create a PCA and plot loadings and correlations
iris.pca <- pcomp(iris[, -5])
vectorplot(loadings(iris.pca))
vectorplot(correlation(iris.pca))
# Note: on screen devices, change aspect ratio of the graph by resizing
# the window to reveal cropped labels...
}

\keyword{aplot}
