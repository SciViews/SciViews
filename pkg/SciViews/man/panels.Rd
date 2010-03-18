\name{panels}
\Rdversion{1.1}
\alias{panel.reg}
\alias{panel.ellipse}
\alias{panel.cor}

\title{More panel plots}

\description{
  Several panel plots that can be used with functions like \code{\link{coplot}}
  and \code{\link{pairs}}.
}

\usage{
panel.reg(x, y, col = par("col"), bg = par("bg"), pch = par("pch"),
    cex = par("cex"), lwd = par("lwd"), line.reg = lm, line.col = "red",
    line.lwd = lwd, untf = TRUE, \dots)
panel.ellipse(x, y, col = par("col"), bg = par("bg"), pch = par("pch"),
    cex = par("cex"), el.level = 0.7, el.col = "cornsilk", el.border = "red",
    major = TRUE, \dots)
panel.cor(x, y, use = "everything", method = c("pearson", "kendall", "spearman"),
    alternative = c("two.sided", "less", "greater"), digits = 2, prefix = "",
    cex = par("cex"), cor.cex = cex, stars.col = "red", \dots)
}

\arguments{
  \item{x}{ a numeric vector. }
  \item{y}{ a numeric vector of same length as \code{x} }
  \item{col}{ the color of the points. }
  \item{bg}{ the background color for symbol used for the points. }
  \item{pch}{ the symbol used for the points. }
  \item{cex}{ the expansion factor used for the points. } 
  \item{lwd}{ the line width. }
   \item{line.reg}{ a function that calculates coefficients of a straight line,
    for instance, \code{\link{lm}}, or \code{\link[MASS]{rlm}}  for robust
    linear regression. }
  \item{line.col}{ the color of the line. }
  \item{line.lwd}{ the width of the line. }
  \item{untf}{ logical asking whether to untransform the straight line in case
    one or both axis are in log scale. }
  \item{el.level}{ the confidence level for the bivariate normal ellipse
    around data; the default value of 0.7 draws an ellipse of roughly +/- 1 sd. }
  \item{el.col}{ the color used to fill the ellipse. }
  \item{el.border}{ the color used to draw the border of the ellipse and the
    standardized major axis. }
  \item{major}{ if \code{TRUE}, the standardized major axis is also drawn. }
  \item{use}{ one of \code{"everything"}, \code{"all.obs"}, /code{"complete.obs"},
    \code{"na.or.complete"}, or \code{"pairwise.complete.obs"} (can be abbreviated).
    Defines how the \code{cor()} function behaves with missing observations. }
  \item{method}{ one of the three correlation coefficients \code{"pearson"},
    (default), \code{"kendall"}, or \code{"spearman"} (can be abbreviated). }
  \item{alternative}{ the alternative hypothesis in correlation test, see
    \code{\link{cor.test}}. }
  \item{digits}{ the number of decimal digits to print when the correlation
    coefficient is printed in the graph. }
  \item{prefix}{ a prefix (character string) to use before the correlation
    coefficient printed in the graph. }
  \item{cor.cex}{ expansion coefficient for text in printing correlation
    coefficients. }
  \item{stars.col}{ the color used for significance stars (with: *** p < 0.001,
    ** p < 0.1, * p < 0.05, . p < 0.1. }
  \item{\dots}{ further arguments to plot functions. }
}

\details{
  Theses functions should be used outside of the diagonal in \code{\link{pairs}()},
  or with \code{\link{coplot}()}, as they are bivariate plots.
}

\value{
  These functions return nothing and are used for their side effect of plotting
  in panels of composite plots.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but code inspired from
  \code{panel.smooth()} in graphics and \code{panel.car()} in package car.
}

\seealso{ \code{\link{coplot}}, \code{\link{pairs}}, \code{\link{panel.smooth}},
  \code{\link{lm}}, \code{\link[ellipse]{ellipse}},
  \code{\link{cor}} and \code{\link{cor.test}} }

\examples{
# Smooth lines in lower graphs and straight lines in upper graphs
pairs(trees, lower.panel = panel.smooth, upper.panel = panel.reg)
# Robust regression lines
require(MASS) # For rlm()
pairs(trees, panel = panel.reg, diag.panel = panel.boxplot,
    reg.line = rlm, line.col = "blue", line.lwd = 2)
# A Double log graph
pairs(trees, lower.panel = panel.smooth, upper.panel = panel.reg, log = "xy")

# Graph suitables to explore correlations (take care that there are potentially
# many simultaneous tests done here... So, you loose much power is the whole
# analysis... use it just as an indication, nothing more!)
# Pearson's r
pairs(trees, lower.panel = panel.ellipse, upper.panel = panel.cor)
# Spearman's rho (ellipse and straight lines not suitable here!)
pairs(trees, lower.panel = panel.smooth, upper.panel = panel.cor,
    method = "spearman", span = 1)
# Several groups (visualize how bad it is to consider the whole set at once!)
pairs(iris[, -5], lower.panel = panel.smooth, upper.panel = panel.cor,
    method = "kendall", span = 1, col = c("red3", "blue3", "green3")[iris$Species])
# Now analyze correlation for one species only
pairs(iris[iris$Species == "virginica", -5], lower.panel = panel.ellipse,
    upper.panel = panel.cor)
    
# A coplot with custom panes
coplot(Petal.Length ~ Sepal.Length | Species, data = iris, panel = panel.ellipse)
}

\keyword{aplot}
