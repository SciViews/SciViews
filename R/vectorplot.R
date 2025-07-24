#' Plot vectors inside a unit circle (PCA loadings or correlations plots).
#'
#'  Plots vectors with 0 < norms < 1 inside a circle. These plots are mainly
#'  designed to represent variables in principal components space for PCAs.
#'
#' @param x An object that has a [SciViews::vectorplot()] method, like
#' 'loadings' or 'correlation', or a numeric vector with 0 < values < 1.
#' @param y A numeric vector with 0 < values < 1 of same length as `x.
#' @param choices A vector of two integers indicating the axes to plot.
#' @param col Color of the arrows and labels.
#' @param circle.col The color for the circle around the vector plot.
#' @param ar.length The length of the arrows.
#' @param pos The position of text relative to arrows. If `NULL`, a suitable
#' position is calculated according to the direction where the arrows are
#' pointing.
#' @param cex The factor of expansion for labels in the graph.
#' @param labels The labels to draw near the arrows.
#' @param main The title of the plot.
#' @param ... Further arguments passed to plot functions.
#' @return The object 'x' is returned invisibly. These functions are called for
#' their side-effect of drawing a vector plot.
#' @export
#' @seealso [SciViews::pcomp()], [stats::loadings()], [SciViews::Correlation()]
#' @keywords aplot
#' @concept Vector and circular plot
#' @examples
#' # Create a PCA and plot loadings and correlations
#' iris.pca <- pcomp(iris[, -5])
#' vectorplot(loadings(iris.pca))
#' vectorplot(Correlation(iris.pca))
#' # Note: on screen devices, change aspect ratio of the graph by resizing
#' # the window to reveal cropped labels...
vectorplot <- function(x, ...)
  UseMethod("vectorplot")

#' @export
#' @rdname vectorplot
vectorplot.default <- function(x, y, col = par("col"), circle.col = "gray",
ar.length = 0.1, pos = NULL, cex = par("cex"), labels = NULL, ...) {
  plot(x, y, type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1, ...)
  abline(h = 0, col = circle.col)
  abline(v = 0, col = circle.col)
  a <- seq(0, 2 * pi, len = 100)
  lines(cos(a), sin(a), col = circle.col)
  arrows(0, 0, x, y, col = col, length = ar.length, ...)
  if (!is.null(labels)) {
    # If pos is NULL, calculate pos for each variable so that label is
    # located outside
    if (is.null(pos))
      pos <- c(2, 1, 4, 3, 2)[floor((atan2(y, x)/pi + 1.25) / 0.5) + 1]
    text(x, y, labels = labels, col = col, pos = pos, cex = cex, ...)
  }
  invisible(list(x = x, y = y))
}

#' @export
#' @rdname vectorplot
vectorplot.loadings <- function(x, choices = 1L:2L, col = par("col"),
circle.col = "gray", ar.length = 0.1, pos = NULL, cex = par("cex"),
labels = rownames(x), main = deparse(substitute(x)), ...) {
  X <- x[, choices]
  vectorplot.default(X[, 1], X[, 2], col = col, circle.col = circle.col,
    ar.length = ar.length, pos = pos, cex = cex, labels = labels,
    main = main, ...)
  invisible(x)
}

#' @export
#' @rdname vectorplot
# Plot vectors inside a circle for correlations along 2 axes (i.e., 2 columns
# in the correlation matrix). This is the typical correlations plot in PCA
vectorplot.Correlation <- function(x, choices = 1L:2L, col = par("col"),
circle.col = "gray", ar.length = 0.1, pos = NULL, cex = par("cex"),
labels = rownames(x), main = deparse(substitute(x)), ...) {
  X <- x[, choices]
  vectorplot.default(X[, 1], X[, 2], col = col, circle.col = circle.col,
    ar.length = ar.length, pos = pos, cex = cex, labels = labels,
    main = main, ...)
  invisible(x)
}
