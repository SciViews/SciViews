\name{correlation}
\alias{correlation}
\alias{correlation.formula}
\alias{correlation.default}
\alias{is.correlation}
\alias{as.correlation}
\alias{print.correlation}
\alias{summary.correlation}
\alias{print.summary.correlation}
\alias{plot.correlation}

\title{Correlation matrices}

\description{
  Compute the correlation matrix between two variables, or more (between all
  columns of a matrix or data frame).
}

\usage{
correlation(x, \dots)
\method{correlation}{formula}(formula, data = NULL, subset, na.action, \dots)
\method{correlation}{default}(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"), \dots)

is.correlation(x)
as.correlation(x)

\method{print}{correlation}(x, digits = 3, cutoff = 0, \dots)
\method{summary}{correlation}(object, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
    symbols = c(" ", ".", ",", "+", "*", "B"), \dots)
\method{print}{summary.correlation}(x, \dots)
\method{plot}{correlation}(x, y = NULL, outline = TRUE,
    cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95), palette = rwb.colors, col = NULL,
    numbers = TRUE, digits = 2, type = c("full", "lower", "upper"),
    diag = (type == "full"), cex.lab = par("cex.lab"), cex = 0.75 * par("cex"),
    \dots)
}

\arguments{
  \item{x}{ a numeric vector, matrix or data frame (or any object for
    \code{is.correlation()}, or \code{as.correlation()}). }
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
  \item{method}{ a character string indicating which correlation coefficient is
    to be computed. One of \code{"pearson"} (default), \code{"kendall"}, or
	\code{"spearman"}, can be abbreviated. }
  \item{y}{ \code{NULL} (default), or a vector, matrix or data frame with
    compatible dimensions to \code{x} for \code{correlation()}. The default is
	equivalent to \code{x = y}, but more efficient. For \code{plot.correlation()},
	if a second 'correlation' object is provided in \code{y}, then a visual
	comparison of two correlation matrices is performed (not implemented yet)! }
  \item{use}{ an optional character string giving a method for computing
    correlations in the presence of missing values. This must be (an abbreviation
	of) one of the strings \code{"everything"}, \code{"all.obs"},
	\code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}. }
  \item{digits}{ digits to print after the decimal separator. }
  \item{cutoff}{ correlation coefficients lower than this (in absolute value) are
    suppressed. }
  \item{object}{ a 'correlation' object. }
  \item{cutpoints}{ the cut points to use for categories. Specify only positive
    values (absolute value of correlation coefficients are summarized, or
	negative equivalents are automatically computed for the graph. Do not include
	0 or 1 in the cutpoints). }
  \item{symbols}{ the symbols to use to summarize the correlation matrix. }
  \item{outline}{ do we draw the outline of the ellipse? }
  \item{palette}{ a function that can produce a palette of colors. }
  \item{col}{ color of the ellipse. If \code{NULL} (default), the colors will be
    computed using \code{cutpoints} and \code{palette}. }
  \item{numbers}{ do we print correlation values in the center of the ellipses? }
  \item{type}{ do we plot a complete matrix, or only lower or upper triangle? }
  \item{diag}{ do we plot items on the diagonal? They have always a correlation
    of one. }
  \item{cex.lab}{ the expansion factor for labels. }
  \item{cex}{ the expansion factor for text. }
  \item{\dots}{ further arguments passed to functions. }
}

\value{
  \code{correlation()} and \code{as.correlation()} create a 'correlation' object,
  while \code{is.correlation()} tests for it.
  
  There are \code{print()} and \code{summary()} methods for the 'correlation'
  object that differ in the symbolic encoding of the correlations in
  \code{summary()}, using \code{\link{symnum}}, which makes large correlation
  matrices more readable.
  
  The method \code{plot} returns nothing, but it draws ellipses on a graph that
  represent the correlation matrix visually. This is essentially the
  \code{plotcorr()} function from package ellipse, with slightly different
  default arguments and with default \code{cutpoints} equivalent to those used
  in the \code{summary} method.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, wrapping code in package ellipse,
  function \code{plotcorr()} for the \code{plot.correlation()} method.
}

\seealso{ \code{\link{cov}}, \code{\link{cov2cor}}, \code{\link{cov.wt}},
  \code{\link[stats]{symnum}}, \code{\link[ellipse]{plotcorr}} and look also at
  \code{\link{panel.cor}} }

\examples{
## This is a simple correlation coefficient
cor(rnorm(10), runif(10))
## but this is a 'correlation' object containing a correlation matrix
correlation(rnorm(10), runif(10))

## 'correlation' objects allow better inspection of the correlation matrices
## than the output of default R cor() function
(longley.cor <- correlation(longley))
summary(longley.cor) # Synthetic view of the correlation matrix
plot(longley.cor)    # Graphical representation

## Use of the formula interface
(mtcars.cor <- correlation(~ mpg + cyl + disp + hp, data = mtcars,
    method = "spearman", na.action = "na.omit"))

mtcars.cor2 <- correlation(mtcars, method = "spearman")
print(mtcars.cor2, cutoff = 0.6)
summary(mtcars.cor2)
plot(mtcars.cor2, type = "lower")

mtcars.cor2["mpg", "cyl"] # Extract one correlation from the correlation matrix
## TODO: a plot comparing two correlation matrices
}

\keyword{distribution}

\concept{ correlation matrix and plot }
