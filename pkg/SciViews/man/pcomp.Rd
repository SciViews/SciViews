\name{pcomp}
\alias{pcomp}
\alias{pcomp.default}
\alias{pcomp.formula}
\alias{print.pcomp}
\alias{summary.pcomp}
\alias{print.summary.pcomp}
\alias{plot.pcomp}
\alias{screeplot.pcomp}
\alias{points.pcomp}
\alias{lines.pcomp}
\alias{text.pcomp}
\alias{biplot.pcomp}
\alias{pairs.pcomp}
\alias{predict.pcomp}
\alias{correlation.pcomp}
\alias{scores}
\alias{scores.pcomp}

\title{Principal Components Analysis}

\description{
  Perform a principal components analysis on a matrix or data frame and return a
  \code{pcomp} object.
}

\usage{
pcomp(x, \dots)
\method{pcomp}{formula}(formula, data = NULL, subset, na.action,
    method = c("svd", "eigen"), \dots)
\method{pcomp}{default}(x, method = c("svd", "eigen"), scores = TRUE,
    center = TRUE, scale = TRUE, tol = NULL, covmat = NULL,
	subset = rep(TRUE, nrow(as.matrix(x))), \dots)

\method{print}{pcomp}(x, \dots)
\method{summary}{pcomp}(object, loadings = TRUE, cutoff = 0.1, \dots)
\method{print}{summary.pcomp}(x, digits = 3, loadings = x$print.loadings,
    cutoff = x$cutoff, \dots)

\method{plot}{pcomp}(x, which = c("screeplot", "loadings", "correlations", "scores"),
    choices = 1L:2L, col = par("col"), bar.col = "gray", circle.col = "gray",
    ar.length = 0.1, pos = NULL, labels = NULL, cex = par("cex"),
    main = paste(deparse(substitute(x)), which, sep = " - "), xlab, ylab, \dots)
\method{screeplot}{pcomp}(x, npcs = min(10, length(x$sdev)), type = c("barplot", "lines"),
    col = "cornsilk", main = deparse(substitute(x)), \dots)
\method{points}{pcomp}(x, choices = 1L:2L, type = "p", pch = par("pch"),
    col = par("col"), bg = par("bg"), cex = par("cex"), \dots)
\method{lines}{pcomp}(x, choices = 1L:2L, groups, type = c("p", "e"),
    col = par("col"), border = par("fg"), level = 0.9, \dots)
\method{text}{pcomp}(x, choices = 1L:2L, labels = NULL, col = par("col"),
    cex = par("cex"), pos = NULL, \dots)
\method{biplot}{pcomp}(x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, \dots)

\method{pairs}{pcomp}(x, choices = 1L:3L, type = c("loadings", "correlations"),
    col = par("col"), circle.col = "gray", ar.col = par("col"), ar.length = 0.05,
    pos = NULL, ar.cex = par("cex"), cex = par("cex"), \dots)

\method{predict}{pcomp}(object, newdata, dim = length(object$sdev), \dots) 
\method{correlation}{pcomp}(x, newvars, dim = length(x$sdev), \dots)
scores(x, \dots)
\method{scores}{pcomp}(x, labels = NULL, dim = length(x$sdev), \dots)
}

\arguments{
  \item{x}{ a matrix or data frame with numeric data. }
  \item{formula}{ a formula with no response variable, referring only to numeric
    variables. }
  \item{data}{ an optional data frame (or similar: see \code{\link{model.frame}})
    containing the variables in the formula \code{formula}. By default the
	variables are taken from \code{environment(formula)}. }
  \item{subset}{ an optional vector used to select rows (observations) of the
    data matrix \code{x}. }
  \item{na.action}{ a function which indicates what should happen when the data
    contain \code{NA}s. The default is set by the \code{na.action} setting of
	\code{\link{options}}, and is \code{\link{na.fail}} if that is unset. The
	'factory-fresh' default is \code{\link{na.omit}}. }
  \item{method}{ either \code{"svd"} (the function uses \code{\link{prcomp}}),
    or \code{"eigen"} (the function uses \code{princomp}), or an abbreviation. }
  \item{\dots}{ arguments passed to or from other methods. If \code{x} is a
    formula one might specify \code{scale}, \code{tol} or \code{covmat}. }
  \item{scores}{ a logical value indicating whether the score on each principal
    component should be calculated. }
  \item{center}{ a logical value indicating whether the variables should be
    shifted to be zero centered. Alternately, a vector of length equal the
	number of columns of \code{x} can be supplied. The value is passed to
	\code{scale}. Note that this argument is ignored for \code{method = "eigen"}
	and the dataset is always centered in this case. }
  \item{scale}{ a logical value indicating whether the variables should be
    scaled to have unit variance before the analysis takes place. The default is
	\code{TRUE}, which in general, is advisable. Alternatively, a vector of
	length equal the number of columns of \code{x} can be supplied. The value is
	passed to \code{scale}. }
  \item{tol}{ only when \code{method = "svd"}. A value indicating the magnitude
    below which components should be omitted. (Components are omitted if their
	standard deviations are less than or equal to \code{tol} times the standard
	deviation of the first component.) With the default null setting, no
	components are omitted. Other settings for tol could be \code{tol = 0} or
	\code{tol = sqrt(.Machine$double.eps)}, which would omit essentially
	constant components. }
  \item{covmat}{ a covariance matrix, or a covariance list as returned by
    \code{\link{cov.wt}} (and \code{\link[MASS]{cov.mve}} or
	\code{\link[MASS]{cov.mcd}} from package MASS). If supplied, this is used
	rather than the covariance matrix of \code{x}. }
  \item{object}{ a 'pcomp' object. }
  \item{loadings}{ do we also summarize the loadings? }
  \item{cutoff}{ the cutoff value below which loadings are replaced by white
    spaces in the table. That way, larger values are easier to spot and to
	read in large tables. }
  \item{digits}{ the number of digits to print. }
  \item{which}{ the graph to plot. }
  \item{choices}{ which principal axes to plot. For 2D graphs, specify two
    integers. }
  \item{col}{ the color to use in graphs. }
  \item{bar.col}{ the color of bars in the screeplot. }
  \item{circle.col}{ the color for the circle in the loadings or correlations
    plots. }
  \item{ar.length}{ the length of the arrows in the loadings and correlations
    plots. }
  \item{pos}{ the position of text relative to arrows in loadings and
    correlations plots. }
  \item{labels}{ the labels to write. If \code{NULL} default values are computed. }
  \item{cex}{ the factor of expansion for text (labels) in the graphs. }
  \item{main}{ the title of the graph. }
  \item{xlab}{ the label of X-axis. }
  \item{ylab}{ the label of Y-axis. }
  \item{pch}{ type of symbol to use. }
  \item{bg}{ background color for symbols. }
  \item{groups}{ a grouping factor. }
  \item{border}{ the color of the border. }
  \item{level}{ the probability level to use to draw the ellipse. }
  \item{pc.biplot}{ do we create a Gabriel's biplot (see \code{\link{biplot}()}
    documentation)? }
  \item{npcs}{ the number of principal components to represent in the screeplot. }
  \item{type}{ the type of screeplot (\code{"barplot"} or \code{"lines"}) or pairs
    plot (\code{"loadings"} or \code{"correlations"}). }
  \item{ar.col}{ color of arrows. }
  \item{ar.cex}{ expansion factor for terxt on arrows. }
  \item{newdata}{ new individuals with observations for the same variables as
    those used for making the PCA. You can then plot these additional
	individuals in the scores graph. }
  \item{newvars}{ new variables with observations for same individuals as those
    used for making the PCA. Correlation with PCs is calculated. You can then
	plot these additional variables in the correlation graph. }
  \item{dim}{ The number of principal components to keep. }
}

\details{
  \code{pcomp()} is a generic function with \code{"formula"} and \code{"default"}
  methods. It is essentially a wrapper around \code{prcomp()} and
  \code{princomp()} to provide a coherent interface and object for both methods. 
  
  A 'pcomp' object is created. It inherits from 'pca' (as in labdsv package, but
  not compatible with the 'pca' object of package ade4!) and of 'princomp'.
  
  For more information on calculation done, refer to \code{\link{prcomp}} for
  \code{method = "svd"} or \code{\link{princomp}} for \code{method = "eigen"}.
}

\value{
  A \code{c("pcomp", "pca", "princomp")} object containing list components:
  \item{comp_i}{Description of comp_i.}
  TODO: complete this (also speak about the various methods)!
}

\note{
  The signs of the columns of the loadings and scores are arbitrary, and so may
  differ between different programs for PCA, and even between different builds
  of \R. 
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but the core code is indeed in
  package stats.
}

\seealso{ \code{\link{vectorplot}}, \code{\link{prcomp}}, \code{\link{princomp}},
  \code{\link{loadings}}, \code{link{correlation}} }

\examples{
## We will analyze mtcars without the Mercedes data (rows 8:14)
data(mtcars)
cars.pca <- pcomp(~mpg+cyl+disp+hp+drat+wt+qsec, data = mtcars, subset = -(8:14))
cars.pca
summary(cars.pca)
screeplot(cars.pca)

## Loadings are extracted and plotted like this
(cars.ldg <- loadings(cars.pca))
plot(cars.pca, which = "loadings") # Equivalent to vectorplot(cars.ldg)

## Similarly, correlations of variables with PCs are extracted and plotted
(cars.cor <- correlation(cars.pca))
plot(cars.pca, which = "correlations") # Equivalent to vectorplot(cars.cor)
## One can add supplementary variables on this graph
lines(correlation(cars.pca,
    newvars = mtcars[-(8:14), c("vs", "am", "gear", "carb")]))

## Plot the scores
plot(cars.pca, which = "scores", cex = 0.8) # Similar to plot(scores(x)[, 1:2])
## Add supplementary individuals to this plot (labels), use also points() or lines()
text(predict(cars.pca, newdata = mtcars[8:14, ]), col = "gray", cex = 0.8)

## More scores plot
## TODO...

## Pairs plot for 3 PCs
iris.pca <- pcomp(iris[, -5])
pairs(iris.pca, col = (2:4)[iris$Species])

## rgl plot for 3 PCs
## TODO...
}

\keyword{ models }

\concept{ principal component analysis and biplot }
