#' More univariate panel plots
#'
#' Several panel plots that can be used with [pairs()].
#'
#' @param x A numeric vector.
#' @param col The color of the points.
#' @param box.col The filling color of the boxplots.
#' @param adjust The bandwidth adjustment factor, see [density()].
#' @param rug Do we add a rug representation (1-d plot) of the points too?
#' @param lwd The line width.
#' @param line.col The color of the line.
#' @param line.lwd The width of the line.
#' @param breaks The number of breaks, the name of a break algorithm, a vector
#' of breakpoints, or any other acceptable value for `breaks` argument of
#' [hist()].
#' @param hist.col The filling color for the histograms.
#' @param hist.border The border color for the histograms.
#' @param hist.density The density for filling lines in the histograms.
#' @param hist.angle The angle for filling lines in the histograms.
#' @param pch The symbol used for the points.
#' @param bg The background color for symbol used for the points.
#' @param cex The expansion factor used for the points.
#' @param qq.pch The symbol used to plot points in the QQ-plots.
#' @param qq.col The color of the symbol used to plot points in the QQ-plots.
#' @param qq.bg The background color of the symbol used to plot points in the
#' QQ-plots.
#' @param qq.cex The expansion factor for points in the QQ-plots.
#' @param qqline.col The color for the QQ-plot lines.
#' @param qqline.lwd The width for the QQ-plot lines.
#' @param ... Further arguments to plot functions, or functions that construct
#' items, like [density()], depending on the context.
#' @return These functions return nothing and are used for their side effect of
#' plotting in panels of composite plots.
#' @details Panel functions [panel_boxplot()], [panel_density()], [panel_hist()]
#' and [panel_qqnorm()] should be used only to plot univariate data on the
#' diagonals of [pairs()] plots (or scatterplot matrix).
#' @author Philippe Grosjean <phgrosjean@sciviews.org>, but code inspired from
#' `spm()` in package **car**.
#' @export
#' @name panels.diag
#' @seealso [pairs()], [boxplot()], [hist()], [density()], [qqnorm()]
#' @keywords aplot
#' @concept panel plots
#' @examples
#' # Example of scatterplot matrices with custom plots on the diagonal
#'
#' # Boxplots
#' pairs(trees, panel = panel_smooth, diag.panel = panel_boxplot)
#' pairs(trees, diag.panel = panel_boxplot, box.col = "gray")
#'
#' # Densities
#' pairs(trees, panel = panel_smooth, diag.panel = panel_density)
#' pairs(trees, diag.panel = panel_density, line.col = "red", adjust = 0.5)
#'
#' # Histograms
#' pairs(trees, panel = panel_smooth, diag.panel = panel_hist)
#' pairs(trees, diag.panel = panel_hist, hist.col = "gray", breaks = "Scott")
#'
#' # QQ-plots against Normal theoretical distribution
#' pairs(trees, panel = panel_smooth, diag.panel = panel_qqnorm)
#' pairs(trees, diag.panel = panel_qqnorm, qqline.col = 2, qq.cex = .5, qq.pch = 3)
panel_boxplot <- function(x, col = par("col"), box.col = "cornsilk", ...) {
  # Note: col is defined here, but unused, because otherwise redefining
  # col would cause an error about duplicated 'col' arguments to boxplot()!
  # further arguments to boxplot are allowed (try notch = TRUE ... not very
  # useful here, but just for test). Note that warnings are generates in
  # pairs() in case of a call with non-graphic arguments, or even, col.box =
  par(new = TRUE)
  boxplot(x, axes = FALSE, col = box.col, horizontal = TRUE,
    xlim = c(0.5, 2), ...)
}

#' @export
#' @rdname panels.diag
panel.boxplot <- panel_boxplot # Backward compatibility

#' @export
#' @rdname panels.diag
panel_density <- function(x, adjust = 1, rug = TRUE, col = par("col"),
lwd = par("lwd"), line.col = col, line.lwd = lwd, ...) {
  # Further arguments to density() are allowed (see examples) but it generates
  # warnings in pairs()
  dens.x <- density(x, adjust = adjust, ...)
  lines(dens.x$x, min(x) + dens.x$y * diff(range(x)) / diff(range(dens.x$y)),
    col = line.col, lwd = line.lwd)
  if (isTRUE(rug))
    points(x, rep(min(x), length(x)), pch = "|", col = line.col)
}

#' @export
#' @rdname panels.diag
panel.density <- panel_density # Backward compatibility

#' @export
#' @rdname panels.diag
panel_hist <- function(x, breaks = "Sturges", hist.col = "cornsilk",
hist.border = NULL, hist.density = NULL, hist.angle = 45, ...) {
  # Here, we try to define all arguments that are specific to the histogram
  # (col, border, density and angle) with specific arguments to allow better
  # control of the appearance of the histograms independently from the other
  # panels
  par(new = TRUE)
  hist(x, breaks = breaks, col = hist.col, border = hist.border,
    density = hist.density, angle = hist.angle, axes = FALSE,
    xlab = "", ylab = "", main = "")
}

#' @export
#' @rdname panels.diag
panel.hist <- panel_hist # Backward compatibility

#' @export
#' @rdname panels.diag
panel_qqnorm <- function(x, pch = par("pch"), col = par("col"), bg = par("bg"),
cex = par("cex"), lwd = par("lwd"), qq.pch = pch, qq.col = col, qq.bg = bg,
qq.cex = cex, qqline.col = qq.col, qqline.lwd = lwd, ...) {
  par(new = TRUE)
  ylim <- range(x, na.rm = TRUE)
  # Leave enough space for name of variables on top of the graph
  ylim[2] <- ylim[2] + (ylim[2] - ylim[1]) / 4
  qqnorm(x, axes = FALSE, xlab = "", ylab = "", main = "",
    ylim = ylim, col = qq.col, bg = qq.bg, pch = qq.pch, cex = qq.cex)
  qqline(x, col = qqline.col, lwd = qqline.lwd, ...)
}

#' @export
#' @rdname panels.diag
panel.qqnorm <- panel_qqnorm # Backward compatibility
