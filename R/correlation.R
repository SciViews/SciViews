#' Correlation matrices
#'
#' Compute the correlation matrix between all columns of a matrix or data frame.
#'
#' @param x A numeric vector, matrix or data frame (or any object for
#' `is.Correlation()` or `as.Correlation()`).
#' @param formula A formula with no response variable, referring only to numeric
#' variables.
#' @param data An optional data frame (or similar, see [model.frame()])
#' containing the variables in the formula. By default the variables
#' are taken from `environment(formula)`.
#' @param subset An optional vector used to select rows (observations) of the
#' data matrix `x`.
#' @param na.action A function which indicates what should happen when the data
#' contain `NA`s. The default is set by the `na.action` setting of `options()`
#' and [na.fail()] is used if that is not set. The 'factory-fresh' default is
#' [na.omit()].
#' @param method A character string indicating which correlation coefficient is
#' to be computed. One of `"pearson"` (default), `"kendall"`, or `"spearman"`,
#' can be abbreviated.
#' @param y `NULL` (default), or a vector, matrix or data frame with compatible
#' dimensions to `x` for `Correlation()`. The default is equivalent to `x = y`,
#' but more efficient.
#' @param use An optional character string giving a method for computing
#' correlations in the presence of missing values. This must be (an abbreviation
#' of) one of the strings `"everything"`, `"all.obs"`, `"complete.obs"`,
#' `"na.or.complete"`, or `"pairwise.complete.obs"`.
#' @param digits Digits to print after the decimal separator.
#' @param cutoff Correlation coefficients lower than this (in absolute value)
#' are suppressed.
#' @param object A 'Correlation' object.
#' @param cutpoints The cut points to use for categories. Specify only positive
#' values (absolute value of correlation coefficients are summarized, or
#' negative equivalents are automatically computed for the graph. Do not include
#' 0 or 1 in the cutpoints).
#' @param symbols The symbols to use to summarize the correlation matrix.
#' @param outline Do we draw the outline of the ellipse?
#' @param palette A function that can produce a palette of colors.
#' @param col Color of the ellipse. If `NULL` (default), the colors will be
#' computed using `cutpoints` and `palette`.
#' @param numbers Do we print correlation values in the center of the ellipses?
#' @param type Do we plot a complete matrix, or only lower or upper triangle?
#' @param diag Do we plot items on the diagonal? They have always a correlation
#' of one.
#' @param cex.lab The expansion factor for labels.
#' @param cex The expansion factor for text.
#' @param choices The items to select.
#' @param lty The line type to draw.
#' @param ar.length The length of the arrow head.
#' @param pos The position relative to arrows.
#' @param labels The label to draw near the arrows.
#' @param ... Further arguments passed to functions.
#' @return `Correlation()` and `as.Correlation()` create a 'Correlation'
#' object, while `is.Correlation()` tests for it.
#'
#' There are `print()` and `summary()` methods for the 'Correlation' object
#' that differ in the symbolic encoding of the correlations,
#' (using [symnum()] for `summary()`), which makes large correlation matrices
#' more readable.
#'
#' The `plot()` method draws ellipses on a graph to represent the correlation
#' matrix visually. This is essentially the [plotcorr()] function from package
#' **ellipse**, with slightly different default arguments and with default
#' `cutpoints` equivalent to those used in the `summary()` method.
#' @author Philippe Grosjean <phgrosjean@sciviews.org>, wrapping code in package
#' **ellipse**, function [plotcorr()] for the `plot.Correlation()` method.
#' @export
#' @seealso [cov()], [cov2cor()], [cov.wt()], [symnum()], [plotcorr()] and look
#' also at [panel_cor()]
#' @keywords distribution
#' @concept correlation matrix and plot
#' @examples
#' # This is a simple correlation coefficient
#' cor(rnorm(10), runif(10))
# but this is a 'Correlation' object containing a correlation matrix
#' Correlation(rnorm(10), runif(10))
#'
#' # 'Correlation' objects allow better inspection of the correlation matrices
#' # than the output of default R cor() function
#' (longley.cor <- Correlation(longley))
#' summary(longley.cor) # Synthetic view of the correlation matrix
#' plot(longley.cor)    # Graphical representation
#'
#' # Use of the formula interface
#' (mtcars.cor <- Correlation(~ mpg + cyl + disp + hp, data = mtcars,
#'   method = "spearman", na.action = "na.omit"))
#'
#' mtcars.cor2 <- Correlation(mtcars, method = "spearman")
#' print(mtcars.cor2, cutoff = 0.6)
#' summary(mtcars.cor2)
#' plot(mtcars.cor2, type = "lower")
#'
#' mtcars.cor2["mpg", "cyl"] # Extract a correlation from the correlation matrix
correlation <- function(x, ...)
  UseMethod("correlation")

#' @export
#' @rdname correlation
Correlation <- correlation # Was defined as correlation, but clash with nlme!

#' @export
#' @rdname correlation
correlation.formula <- function(formula, data = NULL, subset, na.action, ...) {
  mt <- terms(formula, data = data)
  if (attr(mt, "response") > 0L)
    stop("response not allowed in formula")
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf$... <- NULL
  mf[[1L]] <- as.name("model.frame")
  mf <- eval.parent(mf)

  if (.check_vars_numeric(mf))
    stop("Correlation applies only to numerical variables")
  mt <- attr(mf, "terms")
  attr(mt, "intercept") <- 0L
  x <- model.matrix(mt, mf)
  res <- correlation.default(x, ...)
  cl[[1L]] <- as.name("Correlation")
  attr(res, "call") <- cl
  attr(res, "na.method") <- NULL
  if (!is.null(na.action))
    attr(res, "na.action") <- as.character(na.action)
  res
}

#' @export
#' @rdname correlation
correlation.default <- function(x, y = NULL, use = "everything",
method = c("pearson", "kendall", "spearman"), ...) {
  Call <- match.call()
  x <- as.matrix(x)
  na.methods <- c("all.obs", "complete.obs", "pairwise.complete.obs",
    "everything", "na.or.complete")
  na.method <- pmatch(use, na.methods)
  method <- match.arg(method)

	# Just call cor in stats package
  res <- cor(x = x, y = y, use = use, method = method)

  # We want to return a correlation matrix, even if there is one correlation
  if (length(res) == 1) {
    res <- matrix(c(1, res, res, 1), ncol = 2,
      dimnames = list(c("x", "y"), c("x", "y")))
  }

  # Same strings as for cor.test()
  attr(res, "method") <- switch(method,
    pearson = "Pearson's product-moment correlation",
    kendall = "Kendall's rank correlation tau",
    spearman = "Spearman's rank correlation rho",
    method)
  attr(res, "na.method") <- na.methods[na.method]
  attr(res, "call") <- Call
  class(res) <- c("Correlation", "matrix")

  res
}

#' @export
#' @rdname correlation
is.Correlation <- function(x)
  inherits(x, "Correlation")

#' @export
#' @rdname correlation
is.correlation <- is.Correlation # Backward compatibility

#' @export
#' @rdname correlation
as.Correlation <- function(x) {
  if (is.Correlation(x)) return(x)

  # Make sure we have a matrix with numeric data, dimnames and nothing else
  # (drop all other arguments, except 'comment', perhaps)
  res <- structure(as.numeric(x), dim = dim(x), dimnames = dimnames(x))

  # Check that it is a square (2D) matrix, or an atomic number
  d <- dim(x)
  if (is.null(d)) {
    # Is this an atomic number?
    if (length(x) == 1) {
      # Create the simplest correlation matrix using generic 'x' and 'y' labels
      res <- matrix(c(1, res, res, 1), ncol = 2,
        dimnames = list(c("x", "y"), c("x", "y")))
    }
  } else {# Check that it is a square matrix
  if (length(d) != 2 || d[1] != d[2])
    stop("x must be a square matrix")
  }

  rg <- range(res, na.rm = TRUE)
  if (rg[1] < -1 || rg[2] > 1)
    stop("A correlation matrix cannot have values lower than -1 or larger than 1")

  # Reinject comment and other attrinutes, if they exist
  comment(res) <- comment(x)
  attr(res, "method") <- attr(x, "method")
  attr(res, "na.action") <- attr(x, "na.action")
  attr(res, "na.method") <- attr(x, "na.method")

  class(res) <- c("Correlation", "matrix")
  res
}

#' @export
#' @rdname correlation
as.correlation <- as.Correlation # Backward compatibility

#' @export
#' @rdname correlation
print.Correlation <- function(x, digits = 3, cutoff = 0, ...) {
  if (!is.Correlation(x))
    stop("x must be a 'Correlation' object (correlation matrix)")

  method <- attr(x, "method")
  if (is.null(method)) {
    cat("Correlation matrix:\n")
  } else {
    cat("Matrix of ", method, ":\n", sep = "")
  }

  na.method <- attr(x, "na.method")
  if (!is.null(na.method)) {
    cat("(calculation uses ", na.method, ")\n", sep = "")
  } else {
    na.action <- attr(x, "na.action")
    if (!is.null(na.action))
      cat("(missing values are managed with ", na.action, ")\n", sep = "")
  }
  cform <- format(round(x, digits = digits))
  nc <- nchar(cform[1L], type = "c")
  cform[abs(x) < cutoff] <- paste(rep(" ", nc), collapse = "")
  print(cform, quote = FALSE, ...)

  invisible(x)
}

#' @export
#' @rdname correlation
summary.Correlation <- function(object, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
symbols = c(" ", ".", ",", "+", "*", "B"), ...) {
	# Replace the correlation matrix by symbols using symnum()
  res <- symnum(unclass(object), cutpoints = cutpoints, symbols = symbols,
    corr = TRUE, ...)

  # Reinject comment and other attributes, if they exist
  comment(res) <- comment(object)
  attr(res, "method") <- attr(object, "method")
  attr(res, "na.action") <- attr(object, "na.action")
  attr(res, "na.method") <- attr(object, "na.method")

  class(res) <- c("summary.Correlation", "noquote")
  res
}

#' @export
#' @rdname correlation
print.summary.Correlation <- function(x, ...) {
  method <- attr(x, "method")
  if (is.null(method)) {
    cat("Correlation matrix:\n")
  } else {
    cat("Matrix of ", method, ":\n", sep = "")
  }

  na.method <- attr(x, "na.method")
  if (!is.null(na.method)) {
    cat("(calculation uses ", na.method, ")\n", sep = "")
  } else {
    na.action <- attr(x, "na.action")
    if (!is.null(na.action))
      cat("(missing values are managed with ", na.action, ")\n", sep = "")
  }

  print(structure(as.character(x), dim = dim(x), dimnames = dimnames(x),
    legend = attr(x, "legend"), class = "noquote"), ...)

  invisible(x)
}

#' @export
#' @rdname correlation
plot.Correlation <- function(x, y = NULL, outline = TRUE,
cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95), palette = rwb.colors, col = NULL,
numbers = TRUE, digits = 2, type = c("full", "lower", "upper"),
diag = (type == "full"), cex.lab = par("cex.lab"), cex = 0.75 * par("cex"),
...) {
  if (!is.Correlation(x))
    stop("x must be a 'Correlation' object")

  type <- match.arg(type)
  diag <- as.logical(diag[1])
  # Compute colors from cutpoints and palette
  if (is.null(col)) {
    # -1.1 to include -1 - intervals are (,]
    # cutpoints - 0.0001 for positive values to include lower limit instead
    br <- c(-1.1, rev(-cutpoints), cutpoints - 0.0001, 1)
    ct <- cut(x, breaks = br)
    col <- palette(length(levels(ct)))[as.numeric(ct)]
  }

  # Call the plotcorr() function from ellipse package
  plotcorr(x, outline = outline, col = col, numbers = FALSE, type = type,
    diag = diag, cex.lab = cex.lab, cex = cex, ...)
  # Do we print the numbers inside the ellipses?
  if (isTRUE(numbers)) {
    coords <- expand.grid(1:nrow(x), nrow(x):1)
    labels <- format(round(x, digits = digits), digits = digits)
    # Do we plotted only upper or lower triangle and diagonal?
    # Note: we need to invert y-coordinates!
    yinv <- max(coords) + 1 - coords[, 2]
    if (diag) {
      if (type == "lower") {
        # Keep only lower triangle + diagonal
        coords <- coords[coords[, 1] <= yinv, ]
        coords <- coords[order(coords[, 1]), ]
        labels <- labels[lower.tri(labels, diag = TRUE)]
      } else if (type == "upper") {
        # Keep only upper triangle
        coords <- coords[coords[, 1] >= yinv, ]
        coords <- coords[order(coords[, 1]), ]
        labels <- labels[upper.tri(labels, diag = TRUE)]
      }
    } else {# No diagonals
      if (type == "lower") {
        # Keep only lower triangle
        coords <- coords[coords[, 1] < yinv, ]
        coords <- coords[order(coords[, 1]), ]
        labels <- labels[lower.tri(labels)]
      } else if (type == "upper") {
        # Keep only upper triangle
        coords <- coords[coords[, 1] > yinv - 1, ]
        coords <- coords[order(coords[, 1]), ]
        coords[, 2] <- coords[, 2] - 1
        labels <- labels[upper.tri(labels)]
      } else {
        # Plot everything, except diagonal => put test to "" there
        diag(labels) <- ""
      }
    }
    text(coords, labels = labels, cex = cex, ...)
  }
	invisible()
}

#' @export
#' @rdname correlation
lines.Correlation <- function(x, choices = 1L:2L, col = par("col"), lty = 2,
ar.length = 0.1, pos = NULL, cex = par("cex"), labels = rownames(x),  ...) {
  corrs <- x[, choices]
  arrows(0, 0, corrs[, 1], corrs[, 2], col = col, lty = lty,
    length = ar.length, ...)
  if (!is.null(labels)) {
    # If pos is NULL, calculate pos for each variable so that label is
    # located outside
    if (is.null(pos))
      pos <- c(2, 1, 4, 3, 2)[floor((atan2(corrs[, 2], corrs[, 1])/pi +
        1.25) / 0.5) + 1]
    text(corrs, labels = labels, col = col, pos = pos, cex = cex, ...)
  }
  invisible(x)
}
