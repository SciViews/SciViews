\name{panels.diag}
\Rdversion{1.1}
\alias{panel.boxplot}
\alias{panel.density}
\alias{panel.hist}
\alias{panel.qqnorm}

\title{More univariate panel plots}

\description{
  Several panel plots that can be used with function \code{\link{pairs}}.
}

\usage{
panel.boxplot(x, col = par("col"), box.col = "cornsilk", \dots)
panel.density(x, adjust = 1, rug = TRUE, col = par("col"), lwd = par("lwd"),
    line.col = col, line.lwd = lwd, \dots)
panel.hist(x, breaks = "Sturges", hist.col = "cornsilk", hist.border = NULL,
    hist.density = NULL, hist.angle = 45, \dots)
panel.qqnorm(x, pch = par("pch"), col = par("col"), bg = par("bg"),
    cex = par("cex"), lwd = par("lwd"), qq.pch = pch, qq.col = col, qq.bg = bg,
    qq.cex = cex, qqline.col = qq.col, qqline.lwd = lwd, \dots)
}

\arguments{
  \item{x}{ a numeric vector. }
  \item{col}{ the color of the points. }
  \item{box.col}{ the filling color of the boxplots. }
  \item{adjust}{ the bandwidth adjustment factor, see \code{\link{density}}. }
  \item{rug}{ do we add a rug representation (1-d plot) of the points too? }
  \item{lwd}{ the line width. }
  \item{line.col}{ the color of the line. }
  \item{line.lwd}{ the width of the line. }
  \item{breaks}{ the number of breaks, the name of a break algorithm, a vector of
    breakpoints, or any other acceptable value for \code{breaks} argument of
    \code{\link{hist}}}
  \item{hist.col}{ the filling color for the histograms. }
  \item{hist.border}{ the border color for the histograms. }
  \item{hist.density}{ the density for filling lines in the histograms. }
  \item{hist.angle}{ the angle for filling lines in the histograms. }
  \item{pch}{ the symbol used for the points. }
  \item{bg}{ the background color for symbol used for the points. }
  \item{cex}{ the expansion factor used for the points. } 
  \item{qq.pch}{ the symbol used to plot points in the QQ-plots. }
  \item{qq.col}{ the color of the symbol used to plot points in the QQ-plots. }
  \item{qq.bg}{ the background color of the symbol used to plot points in the
    QQ-plots. }
  \item{qq.cex}{ the expansion factor for points in the QQ-plots. }
  \item{qqline.col}{ the color for the QQ-plot lines. }
  \item{qqline.lwd}{ the width for the QQ-plot lines. }
  \item{\dots}{ further arguments to plot functions, or functions that
    construct items, like \code{density()}, depending on the context. }
}

\details{
  Panel functions \code{panel.boxplot()}, \code{panel.density()},
  \code{panel.hist()} and \code{panel.qqnorm()} should be used only to plot
  univariate data on the diagonals of pair plots (or scatterplot matrix).
}

\value{
  These functions return nothing and are used for their side effect of plotting
  in panels of composite plots.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but code inspired from
 \code{spm()} in package car.
}

\seealso{ \code{\link{pairs}}, \code{\link{boxplot}}, \code{\link{hist}},
  \code{\link{density}}, \code{\link{qqnorm}} }

\examples{
# Example of scatterplot matrices with custom plots on the diagonal
# Boxplots
pairs(trees, panel = panel.smooth, diag.panel = panel.boxplot)
pairs(trees, diag.panel = panel.boxplot, box.col = "gray")
# Densities
pairs(trees, panel = panel.smooth, diag.panel = panel.density)
pairs(trees, diag.panel = panel.density, line.col = "red", adjust = 0.5)
# Histograms
pairs(trees, panel = panel.smooth, diag.panel = panel.hist)
pairs(trees, diag.panel = panel.hist, hist.col = "gray", breaks = "Scott")
# QQ-plots against Normal theoretical distribution
pairs(trees, panel = panel.smooth, diag.panel = panel.qqnorm)
pairs(trees, diag.panel = panel.qqnorm, qqline.col = 2, qq.cex = .5, qq.pch = 3)
}

\keyword{aplot}
