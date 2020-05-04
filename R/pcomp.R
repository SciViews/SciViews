#' Principal Components Analysis
#'
#' Perform a principal components analysis (PCA) on a matrix or data frame and
#' return a `pcomp` object.
#'
#' @param x A matrix or data frame with numeric data.
#' @param formula A formula with no response variable, referring only to numeric
#' variables.
#' @param data An optional data frame (or similar, see [model.frame()])
#' containing the variables in the formula. By default the variables
#' are taken from `environment(formula)`.
#' @param subset An optional vector used to select rows (observations) of the
#' data matrix `x`.
#' @param na.action A function which indicates what should happen when the data
#' contain `NA`s. The default is set by the `na.action` setting of
#' [options()], and is [na.fail()] if that is  not set. The 'factory-fresh'
#' default is [na.omit()].
#' @param method Either `"svd"` (using [prcomp()]), `"eigen"` (using
#' [princomp()]), or an abbreviation.
#' @param ... Arguments passed to or from other methods. If `x` is a
#' formula one might specify `scale`, `tol` or `covmat`.
#' @param scores A logical value indicating whether the score on each principal
#' component should be calculated.
#' @param center A logical value indicating whether the variables should
#' centered. Alternately, a vector of length equal the number of columns of `x`
#' can be supplied. The value is passed to `scale`.
#' Note that this argument is ignored for `method = "eigen"` and the dataset is
#' always centered in this case.
#' @param scale A logical value indicating whether the variables should be
#' scaled to have unit variance before the analysis takes place. The default is
#' `TRUE`, which in general, is advisable. Alternatively, a vector of length
#' equal the number of columns of `x` can be supplied. The value is passed to
#' [scale()].
#' @param tol Only when `method = "svd"`. A value indicating the magnitude
#' below which components should be omitted. (Components are omitted if their
#' standard deviations are less than or equal to `tol` times the standard
#' deviation of the first component.) With the default null setting, no
#' components are omitted. Other settings for `tol =` could be `tol = 0` or
#' `tol = sqrt(.Machine$double.eps)`, which would omit essentially constant
#' components.
#' @param covmat A covariance matrix, or a covariance list as returned by
#' [cov.wt()] (and [cov.mve()] or [cov.mcd()] from package **MASS**). If
#' supplied, this is used rather than the covariance matrix of `x`.
#' @param object A 'pcomp' object.
#' @param loadings Do we also summarize the loadings?
#' @param cutoff The cutoff value below which loadings are replaced by white
#' spaces in the table. That way, larger values are easier to spot and to read
#' in large tables.
#' @param digits The number of digits to print.
#' @param which The graph to plot.
#' @param choices Which principal axes to plot. For 2D graphs, specify two
#' integers.
#' @param col The color to use in graphs.
#' @param bar.col The color of bars in the screeplot.
#' @param circle.col The color for the circle in the loadings or correlations
#' plots.
#' @param ar.length The length of the arrows in the loadings and correlations
#' plots.
#' @param pos The position of text relative to arrows in loadings and
#' correlation plots.
#' @param labels The labels to write. If `NULL` default values are computed.
#' @param cex The factor of expansion for text (labels) in the graphs.
#' @param main The title of the graph.
#' @param xlab The label of the x-axis.
#' @param ylab The label of the y-axis.
#' @param pch The type of symbol to use.
#' @param bg The background color for symbols.
#' @param groups A grouping factor.
#' @param border The color of the border.
#' @param level The probability level to use to draw the ellipse.
#' @param pc.biplot Do we create a Gabriel's biplot (see [biplot()])?
#' @param npcs The number of principal components to represent in the screeplot.
#' @param type The type of screeplot (`"barplot"` or `"lines"`) or pairs plot
#' (`"loadings"` or `"correlations"`).
#' @param ar.col Color of arrows.
#' @param ar.cex Expansion factor for text on arrows.
#' @param newdata New individuals with observations for the same variables as
#' those used for calculating the PCA. You can then plot these additional
#' individuals in the scores plot.
#' @param newvars New variables with observations for same individuals as those
#' used for calculating the PCA. Correlation with PCs is calculated. You can
#' then plot these additional variables in the correlation plot.
#' @param dim The number of principal components to keep.
#' @return A `c("pcomp", "pca", "princomp")` object.
#' @details `pcomp()` is a generic function with `"formula"` and `"default"`
#' methods. It is essentially a wrapper around [prcomp()] and [princomp()] to
#' provide a coherent interface and object for both methods.
#'
#' A 'pcomp' object is created. It inherits from 'pca' (as in **labdsv**
#' package, but not compatible with the version of 'pca' in **ade4**) and of
#' 'princomp'.
#'
#' For more information on algorithms, refer to [prcomp()] for
#' `method = "svd"` or [princomp()] for `method = "eigen"`.
#' @note The signs of the columns for the loadings and scores are arbitrary. So,
#' they could differ between functions for PCA, and even between different
#' builds of \R.
#' @author Philippe Grosjean <phgrosjean@sciviews.org>, but the core code is
#' indeed in package **stats**.
#' @export
#' @seealso [prcomp()], [princomp()], [loadings()], [vectorplot()],
#' [Correlation()]
#' @keywords models
#' @concept principal component analysis and biplot
#' @examples
#' # Let's analyze mtcars without the Mercedes data (rows 8:14)
#' data(mtcars)
#' cars.pca <- pcomp(~ mpg + cyl + disp + hp + drat + wt + qsec,
#'   data = mtcars, subset = -(8:14))
#' cars.pca
#' summary(cars.pca)
#' screeplot(cars.pca)
#'
#' # Loadings are extracted and plotted this way:
#' (cars.ldg <- loadings(cars.pca))
#' plot(cars.pca, which = "loadings") # Equivalent to vectorplot(cars.ldg)
#'
#' # Similarly, correlations of variables with PCs are extracted and plotted:
#' (cars.cor <- Correlation(cars.pca))
#' plot(cars.pca, which = "correlations") # Equivalent to vectorplot(cars.cor)
#' # One can add supplementary variables on this graph
#' lines(Correlation(cars.pca,
#'   newvars = mtcars[-(8:14), c("vs", "am", "gear", "carb")]))
#'
#' # Plot the scores:
#' plot(cars.pca, which = "scores", cex = 0.8) # Similar to plot(scores(x)[, 1:2])
#' # Add supplementary individuals to this plot (labels), also points() or lines()
#' text(predict(cars.pca, newdata = mtcars[8:14, ]),
#'   labels = rownames(mtcars[8:14, ]), col = "gray", cex = 0.8)
#'
#' # Pairs plot for 3 PCs
#' iris.pca <- pcomp(iris[, -5])
#' pairs(iris.pca, col = (2:4)[iris$Species])
pcomp <- function(x, ...)
  UseMethod("pcomp")

#' @export
#' @rdname pcomp
pcomp.formula <- function(formula, data = NULL, subset, na.action,
method = c("svd", "eigen"), ...) {
  # Largely inspired from prcomp.formula
  mt <- terms(formula, data = data)
  if (attr(mt, "response") > 0L)
    stop("response not allowed in formula")
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf$... <- NULL
  mf[[1L]] <- as.name("model.frame")
  mf <- eval.parent(mf)
  if (.check_vars_numeric(mf))
    stop("PCA applies only to numerical variables")
  na.act <- attr(mf, "na.action")
  mt <- attr(mf, "terms")
  attr(mt, "intercept") <- 0L
  x <- model.matrix(mt, mf)
  res <- pcomp.default(x, ...)
  cl[[1L]] <- as.name("pcomp")
  res$call <- cl
  if (!is.null(na.act)) {
    res$na.action <- na.act
    if (!is.null(sc <- res$x))
      res$x <- napredict(na.act, sc)
  }
  res
}

.svd_pca <- function(x, retx, center, scale, tol, ...) {
pca <- prcomp(x, retx = retx, center = center, scale = scale, tol = tol, ...)
  # Required by pcomp.default()
  # Rework the result to make it fit in the "pcomp" object
  names(pca$sdev) <- paste("PC", 1:length(pca$sdev), sep = "")
  if (isTRUE(!pca$center)) {
    pca$center <- rep(0, length(pca$sdev))
    names(pca$center) <- colnames(pca$rotation)
  }
  if (isTRUE(!pca$scale)) {
    pca$scale <- rep(1, length(pca$sdev))
    names(pca$scale) <- colnames(pca$rotation)
  }
  rn <- rownames(x)
  if (is.null(rn)) {
    rownames(pca$x) <- as.character(1:nrow(pca$x))
  } else {
    rownames(pca$x) <- rn
  }
  res <- list(
    loadings = structure(pca$rotation, class = "loadings"),
    scores = if (is.null(pca$x)) NULL else as.data.frame(pca$x),
    sdev = pca$sdev,
    totdev = sqrt(sum(pca$sdev^2)),
    n.obs = nrow(pca$x),
    center = pca$center,
    scale = pca$scale,
    method = "svd"
  )
  res
}

.eigen_pca <- function(x, cor, scores, covmat = NULL, subset, ...) {
  # Required by pcomp.default()
  if (is.null(covmat)) {
    pca <- princomp(x, cor = cor, scores = scores, subset = subset, ...)
  } else {
    pca <- princomp(cor = cor, scores = scores, covmat = covmat,
      subset = subset, ...)
  }
  n <- length(pca$sdev)
  pc <- paste("PC", 1:n, sep = "")  # rename Comp.1, ... in PC1, ...
  names(pca$sdev) <- pc
  colnames(pca$loadings) <- pc
  if (!is.null(pca$scores)) {
    colnames(pca$scores) <- pc
    # If there are rownames to x, use it
    rn <- rownames(x)
    if (is.null(rn)) {
      rownames(pca$scores) <- as.character(1:nrow(pca$scores))
    } else {
      rownames(pca$scores) <- rn
    }
    pca$scores <- as.data.frame(pca$scores)
  }
  res <- list(
    loadings = pca$loadings,
    scores = pca$scores,
    sdev = pca$sdev,
    totdev = sum(pca$sdev),
    n.obs = pca$n.obs,
    center = pca$center,
    scale = pca$scale,
    method = "eigen"
  )
  res
}

#' @export
#' @rdname pcomp
pcomp.default <- function(x, method = c("svd", "eigen"), scores = TRUE,
center = TRUE, scale = TRUE, tol = NULL, covmat = NULL,
subset = rep(TRUE, nrow(as.matrix(x))), ...) {
  # Perform a PCA, either using prcomp (method = "svd"), or princomp ("eigen")
  cl <- match.call()
  cl[[1L]] <- as.name("pcomp")

  # Check that all variables are numeric (otherwise, issue a clear message)!
  x <- as.data.frame(x)
  if (!all(sapply(x, is.numeric)))
    stop("Cannot perform a PCA: one or more variables are not numeric.")

  method <- match.arg(method)
  if (method == "eigen" && !isTRUE(center))
    warning("For method 'eigen', center is always TRUE")
  res <- switch(method,
    svd = .svd_pca(x, retx = scores, center = center, scale = scale,
      tol = tol, ...),
    eigen = .eigen_pca(x, cor = scale, scores = scores, covmat = covmat,
      subset = subset, ...),
    stop("method must be either 'svd' or 'eigen'")
  )

  res$call <- cl
  # We return a specific object, but it is compatible (i.e., overloads), both
  # "pca" in package labdsv and "princomp" in package stats
  class(res) <- c("pcomp", "pca", "princomp")
  res
}

#' @export
#' @rdname pcomp
print.pcomp <- function(x, ...) {
  # similar to print.princomp, but reports variances instead of sds
  cat("Call:\n")
  dput(x$call, control = NULL)
  cat("\nVariances:\n")
  print(x$sdev^2, ...)
  cat("\n", length(x$scale), " variables and ", x$n.obs, "observations.\n")
  invisible(x)
}

#' @export
#' @rdname pcomp
# print method (similar to print.princomp, but reports variances instead of sds)
# summary method (same as summary.princomp, but with TRUE for loadings)
summary.pcomp <- function(object, loadings = TRUE, cutoff = 0.1, ...) {
  object$cutoff <- cutoff
  object$print.loadings <- loadings
  class(object) <- "summary.pcomp"
  object
}

#' @export
#' @rdname pcomp
# print method for summary.pcomp object (slightly modified from princomp)
print.summary.pcomp <- function(x, digits = 3, loadings = x$print.loadings,
cutoff = x$cutoff, ...) {
  vars <- x$sdev^2
  vars <- vars/sum(vars)
  cat("Importance of components (eigenvalues):\n")
  print(rbind(`Variance` = round(x$sdev^2, 5),
    `Proportion of Variance` = round(vars, 5),
    `Cumulative Proportion` = round(cumsum(vars), 5)), digits = digits, ...)
  if (loadings) {
    cat("\nLoadings (eigenvectors, rotation matrix):\n")
    cx <- format(round(x$loadings, digits = digits))
    cx[abs(x$loadings) < cutoff] <- paste(rep(" ", nchar(cx[1, 1],
      type = "w")), collapse = "")
    print(cx, quote = FALSE, ...)
  }
  invisible(x)
}

.plot_scores <- function(x, choices, col, circle.col, labels, cex, main,
xlab, ylab, ...) {
  # Required by plot.pcomp()
  if (is.null(x$scores))
    stop("no scores are available: refit with 'scores = TRUE'")
  if (is.null(labels)) {
    labels <- rownames(x$scores)
    if (is.null(labels))  # If still no labels
      labels <- as.character(1:nrow(x$scores))
  } else if (!isTRUE(!as.numeric(labels))) {
    labels <- as.character(labels)
  }
  scores <- scores(x)[, choices]
  plot(scores, type = "n", asp = 1, main = main, xlab = xlab, ylab = ylab)
  abline(h = 0, col = circle.col)
  abline(v = 0, col = circle.col)
  if (!isTRUE(!as.numeric(labels)))
    text(scores, labels = labels, col = col, cex = cex, ...)
}


#' @export
#' @rdname pcomp
plot.pcomp <- function(x,
which = c("screeplot", "loadings", "correlations", "scores"), choices = 1L:2L,
col = par("col"), bar.col = "gray", circle.col = "gray", ar.length = 0.1,
pos = NULL, labels = NULL, cex = par("cex"),
main = paste(deparse(substitute(x)), which, sep = " - "), xlab, ylab, ...) {
  which <- match.arg(which)
  main <- main[1]
  # Calculate default xlab and ylab
  labs <- paste(names(x$sdev), " (", round((x$sdev^2 / x$totdev^2) * 100,
    digits = 1), "%)", sep = "")
  if (missing(xlab)) xlab <- labs[choices[1]] else xlab
  if (missing(ylab)) ylab <- labs[choices[2]] else ylab
  switch(which,
    screeplot = screeplot(unclass(x), col = bar.col, main = main, ...),
    loadings = vectorplot(loadings(x), choices = choices, col = col,
      circle.col = circle.col, ar.length = ar.length, pos = pos, cex = cex,
      labels = if (is.null(labels)) rownames(loadings(x)) else labels,
      main = main, xlab = xlab, ylab = ylab, ...),
    correlations = vectorplot(Correlation(x), choices = choices, col = col,
      circle.col = circle.col, ar.length = ar.length, pos = pos, cex = cex,
      labels = if (is.null(labels)) rownames(loadings(x)) else labels,
      main = main, xlab = xlab, ylab = ylab, ...),
    scores = .plot_scores(x, choices = choices, col = col, cex = cex,
      circle.col = circle.col, labels = labels, main = main,
      xlab = xlab, ylab = ylab, ...),
    stop("unknown plot type")
  )
}

#' @export
#' @rdname pcomp
screeplot.pcomp <- function(x, npcs = min(10, length(x$sdev)),
type = c("barplot", "lines"), col = "cornsilk", main = deparse(substitute(x)),
...) {
  # screeplot() method (add cumulative variance curve to the plot)
  force(main)
  type <- match.arg(type)
  pcs <- x$sdev^2
  xp <- seq_len(npcs)
  if (type == "barplot") {
    barplot(pcs[xp], names.arg = names(pcs[xp]), main = main,
      ylab = "Variances", col = col, ...)
  } else {
    plot(xp, pcs[xp], type = "b", axes = FALSE, main = main,
      xlab = "", ylab = "Variances", ...)
    axis(2)
    axis(1, at = xp, labels = names(pcs[xp]))
  }
  invisible()
}

#' @export
#' @rdname pcomp
points.pcomp <- function(x, choices = 1L:2L, type = "p", pch = par("pch"),
col = par("col"), bg = par("bg"), cex = par("cex"), ...) {
	if (is.null(x$scores))
		stop("no scores are available: refit with 'scores = TRUE'")
	points(scores(x)[, choices], type = type, pch = pch, col = col, bg = bg,
		cex = cex, ...)
}

.polygons <- function(scores, groups, n, col, border, ...) {
  # Required by lines.pcomp()
  for (i in 1:n) {
    sc <- na.omit(scores[as.numeric(groups) == i, ])
    if (NROW(sc) > 1) {
      pts <- chull(sc)
      # Close polygon
      pts <- c(pts, pts[1])
      polygon(sc[pts, 1], sc[pts, 2], col = col[i],
        border = border[i], ...)
    }
  }
}

.ellipses <- function(scores, groups, n, col, border, level, ...) {
  # Required by lines.pcomp()
  for (i in 1:n) {
    sc <- na.omit(scores[as.numeric(groups) == i, ])
    if (NROW(sc) > 1) {
      x <- sc[, 1]
      y <- sc[, 2]
      polygon(ellipse(cor(x, y), scale = c(sd(x), sd(y)),
        centre = c(mean(x), mean(y)), level = level), col = col[i],
        border = border[i], ...)
    }
  }
}

#' @export
#' @rdname pcomp
lines.pcomp <- function(x, choices = 1L:2L, groups, type = c("p", "e"),
col = par("col"), border = par("fg"), level = 0.9, ...) {
  # Use groups to draw either polygons or ellipses for each group
  if (is.null(x$scores))
    stop("no scores are available: refit with 'scores = TRUE'")
  if (missing(groups))
    stop("you must provide groups")
  scores <- x$scores[, choices]
  groups <- as.factor(groups)
  n <- length(levels(groups))
  col <- rep(col, length.out = n)
  border <- rep(border, length.out = n)
  type <- match.arg(type)
  switch(type,
    p = .polygons(scores, groups = groups, n = n, col = col,
      border = border, ...),
    e = .ellipses(scores, groups = groups, n = n, col = col,
      border = border, level = level, ...),
    stop("unknown type, currently only 'p' for polygons et 'e' for ellipses")
  )
}

#' @export
#' @rdname pcomp
text.pcomp <- function(x, choices = 1L:2L, labels = NULL, col = par("col"),
cex = par("cex"), pos = NULL, ...) {
  if (is.null(x$scores))
    stop("no scores are available: refit with 'scores = TRUE'")
  if (is.null(labels))
    labels <- as.character(1:nrow(x$scores))
  text(x$scores[, choices], labels = labels, col = col, cex = cex,
    pos = pos, ...)
}

#' @export
#' @rdname pcomp
biplot.pcomp <- function(x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, ...) {
  if (length(choices) != 2)
    stop("length of choices must be 2")
  if (!length(scores <- x$scores))
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
      domain = NA)
  if (is.complex(scores))
    stop("biplots are not defined for complex PCA")
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1)
    warning("'scale' is outside [0, 1]")
  if (scale != 0) {
    lam <- lam^scale
  } else {
    lam <- 1
  }
  if (pc.biplot)
    lam <- lam/sqrt(n)
  biplot(unclass(t(t(scores[, choices])/lam)),
    t(t(x$loadings[, choices]) * lam), ...)
}

.panel_individuals <- function(x, y, ...) {
  # Required by pairs.pcomp()
  pos <- 1:(which(is.nan(x))[1] - 1)
  points(x[pos], y[pos], ...)
}

.panel_variables <- function(x, y, ar.labels, ar.col, ar.cex, labels, col,
cex, ...) {
  # Required by pairs.pcomp()
  pos <- (which(is.nan(x))[1] + 1):length(x)
  par(new = TRUE)
  # We want to invert position of x and y here to get same one as indivs
  vectorplot(y[pos], x[pos], axes = FALSE, labels = ar.labels, col = ar.col,
    cex = ar.cex, ...)
}

#' @export
#' @rdname pcomp
pairs.pcomp <- function(x, choices = 1L:3L,
type = c("loadings", "correlations"), col = par("col"), circle.col = "gray",
ar.col = par("col"), ar.length = 0.05, pos = NULL, ar.cex = par("cex"),
cex = par("cex"), ...) {
  type <- match.arg(type)
  X <- scores(x)[, choices]
  labs <- paste(names(x$sdev), " (", round((x$sdev^2 / x$totdev^2) * 100,
    digits = 1), "%)", sep = "")[choices]
  # Add a row of NaN to separate indivs and vars
  X <- rbind(X, rep(NaN, length(choices)))
  # Add vars
  vars <- switch(type,
    loadings = loadings(x)[, choices],
    correlations = Correlation(x)[, choices]
  )
  X <- rbind(X, vars)
  names(X) <- labs
  # TODO: Why do I get warning with non par arguments?!
  suppressWarnings(pairs(X, lower.panel = .panel_individuals,
    upper.panel = .panel_variables,
    col = col, circle.col = circle.col, ar.col = ar.col, ar.cex = ar.cex,
    ar.length = ar.length, ar.labels = rownames(vars), pos = pos,
    cex = cex, ...))
}

#' @export
#' @rdname pcomp
predict.pcomp <- function(object, newdata, dim = length(object$sdev), ...) {
  if (dim > length(object$sdev)) {
    warning("Only", length(object$sdev), " axes available\n")
      dim <- length(object$sdev)
  }
  if (missing(newdata)) {
    if (!is.null(object$scores)) {
      return(object$scores[, 1:dim])
    } else {
      stop("no scores are available: refit with 'scores = TRUE'")
    }
  }
  if (length(dim(newdata)) != 2L)
    stop("'newdata' must be a matrix or data frame")
  nm <- rownames(object$loadings)
  if (!is.null(nm)) {
    if (!all(nm %in% colnames(newdata)))
      stop("'newdata' does not have named columns matching one or more of the original columns")
    newdata <- newdata[, nm, drop = FALSE]
  } else {
    if (NCOL(newdata) != NROW(object$loadings))
      stop("'newdata' does not have the correct number of columns")
  }
  scale(newdata, object$center, object$scale) %*% object$loadings[, 1:dim]
}

#' @export
#' @rdname pcomp
correlation.pcomp <- function(x, newvars, dim = length(x$sdev), ...) {
  # Extract Correlation from a pcomp object. If newvar is provided, it
  # calculates correlations between this new variable and corresponding PCs
  # (providing that scores were calculated, and that the nrow() of new
  # variable is the same as nrow(scores), assumed to be the same individuals
  # as in the original PCA)
  Call <- match.call()

  dim <- as.integer(dim)[1]
  if (dim > length(x$sdev)) {
    warning("Only", length(x$sdev), " axes available\n")
    dim <- length(x$sdev)
  }
  dims <- 1:dim

  if (missing(newvars)) {
    # Just extract correlations (calculated after loadings)
    if (is.null(loads <- loadings(x))) {
      return(NULL)
    } else {
      res <- sweep(loads[, dims], 2,  x$sdev[dims], "*")
      # Create a 'Correlation' object with this
      attr(res, "method") <- "PCA variables and components correlation"
      attr(res, "call") <- Call
      class(res) <- c("Correlation", "matrix")
      return(res)
    }
  } else {
    # Calculate correlation of new variables with PCs
    # Must have same number of observations as in scores, otherwise, we got
    # the error message: "incompatible dimensions"
    if (is.null(scores <- x$scores))
      stop("no scores are available: refit with 'scores = TRUE'")
    # TODO: if these are rownames, check that they match
    res <- Correlation(newvars, scores[, dims])
    attr(res, "method") <- "PCA variables and components correlation"
    return(res)
  }
}

#' @export
#' @rdname pcomp
scores <- function(x, ...) {
  # A generic function compatible with the corresponding one in labdsv package
  UseMethod("scores")
}

#' @export
#' @rdname pcomp
scores.pcomp <- function(x, labels = NULL, dim = length(x$sdev), ...) {
  # Borrowed from scores in labdsv
  # but returns a data frame instead of a matrix
  if (dim > length(x$sdev)) {
    warning("Only", length(x$sdev), " axes available\n")
      dim <- length(x$sdev)
  }
  if (is.null(x$scores))
    stop("no scores are available: refit with 'scores = TRUE'")
  if (!is.null(labels)) {
	  as.data.frame(cbind(x$scores[, 1:dim], labels))
  } else {
    as.data.frame(x$scores[, 1:dim])
  }
}
