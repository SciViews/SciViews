#' More panel plots.
#'
#' Several panel plots that can be used with functions like [coplot()] and
#' [pairs))].
#'
#' @param x A numeric vector.
#' @param y A numeric vector of same length as `x`.
#' @param col The color of the points.
#' @param bg The background color for symbol used for the points.
#' @param pch The symbol used for the points.
#' @param cex The expansion factor used for the points.
#' @param lwd The line width.
#' @param line.reg A function that calculates coefficients of a straight line,
#' for instance, [lm()], or [rlm()] for robust linear regression.
#' @param line.col The color of the line.
#' @param line.lwd The width of the line.
#' @param untf Logical asking whether to untransform the straight line in case
#' one or both axis are in log scale.
#' @param el.level The confidence level for the bivariate normal ellipse around
#' data; the default value of 0.7 draws an ellipse of roughly +/-1 sd.
#' @param el.col The color used to fill the ellipse.
#' @param el.border The color used to draw the border of the ellipse and the
#' standardized major axis.
#' @param major If `TRUE`, the standardized major axis is also drawn.
#' @param use One of `"everything"`, `"all.obs"`, `"complete.obs"`,
#' `"na.or.complete"`, or `"pairwise.complete.obs"` (can be abbreviated).
#' Defines how the [cor()] function behaves with missing observations.
#' @param method One of the three correlation coefficients `"pearson"`
#' (default), `"kendall"`, or `"spearman"`. Can be abbreviated.
#' @param alternative The alternative hypothesis in correlation test, see
#' [cor.test()].
#' @param digits  The number of decimal digits to print when the correlation
#' coefficient is printed in the graph.
#' @param prefix A prefix (character string) to use before the correlation
#' coefficient printed in the graph.
#' @param cor.cex Expansion coefficient for text in printing correlation
#' coefficients.
#' @param stars.col The color used for significance stars (with: *** p < 0.001,
#' ** p < 0.1, * p < 0.05, . p < 0.1.
#' @param col.smooth Color to be used by lines for drawing the smooths.
#' @param span Smoothing parameter f for [lowess()], see there.
#' @param iter Number of robustness iterations for [lowess()].
#' @param ... Further arguments to plot functions.
#' @return These functions return nothing and are used for their side effect of
#' plotting in panels of composite plots.
#' @details Theses functions should be used outside of the diagonal in
#' [pairs()], or with [coplot()], as they are bivariate plots.
#' @author Philippe Grosjean <phgrosjean@sciviews.org>, but code inspired from
#' [panel.smooth()] in **graphics** and `panel.car()` in package **car**.
#' @export
#' @name panels
#' @seealso [coplot()], [pairs()], [panel.smooth()], [lm()], [ellipse()],
#' [cor()] and [cor.test()]
#' @keywords aplot
#' @concept panel plots
#' @examples
#' # Smooth lines in lower graphs and straight lines in upper graphs
#' pairs(trees, lower.panel = panel_smooth, upper.panel = panel_reg)
#' # Robust regression lines
#' library(MASS)  # For rlm()
#' pairs(trees, panel = panel_reg, diag.panel = panel_boxplot,
#'   reg.line = rlm, line.col = "blue", line.lwd = 2)
#' # A Double log graph
#' pairs(trees, lower.panel = panel_smooth, upper.panel = panel_reg, log = "xy")
#'
#' # Graph suitables to explore correlations (take care there are potentially
#' # many simultaneous tests done here... So, you loose much power in the whole
#' # analysis... use it just as an indication!)
#' # Pearson's r
#' pairs(trees, lower.panel = panel_ellipse, upper.panel = panel_cor)
#' # Spearman's rho (ellipse and straight lines not suitable here!)
#' pairs(trees, lower.panel = panel_smooth, upper.panel = panel_cor,
#'   method = "spearman", span = 1)
#' # Several groups (visualize how bad it is to consider the whole set at once!)
#' pairs(iris[, -5], lower.panel = panel_smooth, upper.panel = panel_cor,
#'   method = "kendall", span = 1,
#'   col = c("red3", "blue3", "green3")[iris$Species])
#' # Now analyze correlation for one species only
#' pairs(iris[iris$Species == "virginica", -5], lower.panel = panel_ellipse,
#'   upper.panel = panel_cor)
#'
#' # A coplot with custom panes
#' coplot(Petal.Length ~ Sepal.Length | Species, data = iris,
#'   panel = panel_ellipse)
panel_reg <- function(x, y, col = par("col"), bg = par("bg"), pch = par("pch"),
cex = par("cex"), lwd = par("lwd"), line.reg = lm, line.col = "red",
line.lwd = lwd, untf = TRUE, ...) {
  # Inspired from panel.car() in car package, but without smooth line...
  points(x, y, col = col, bg = bg, pch = pch, cex = cex)
  if (is.function(line.reg))
    abline(reg = line.reg(y ~ x), col = line.col, lwd = line.lwd,
      untf = untf, ...)
}

#' @export
#' @rdname panels
panel.reg <- panel_reg # Backward compatibility

#' @export
#' @rdname panels
panel_ellipse <- function(x, y, col = par("col"), bg = par("bg"),
pch = par("pch"), cex = par("cex"), el.level = 0.7, el.col = "cornsilk",
el.border = "red", major = TRUE, ...) {
  el <- ellipse(cor(x, y, use = "complete.obs"), scale = c(sd(x), sd(y)),
    centre = c(mean(x), mean(y)), level = el.level)
  polygon(el, col = el.col, border = el.border)
  if (isTRUE(major)) {
    # b is the slope of the standardized major axis
    d <- na.omit(data.frame(y, x))
    v <- cov(d) * (nrow(d) - 1)
    b <- sign(v[1, 2]) * sqrt(v[1, 1] / v[2, 2])
    a <- mean(y, na.rm = TRUE) - b * mean(x, na.rm = TRUE)
    abline(a = a, b = b, col = el.border, ...)
  }
  points(x, y, col = col, bg = bg, pch = pch, cex = cex)
}

#' @export
#' @rdname panels
panel.ellipse <- panel_ellipse # Backward compatibility

#' @export
#' @rdname panels
panel_cor <- function(x, y, use = "everything",
method = c("pearson", "kendall", "spearman"),
alternative = c("two.sided", "less", "greater"), digits = 2, prefix = "",
cex = par("cex"), cor.cex = cex, stars.col = "red", ...) {
  # One way to visualize correlation coefficients, inspired from
  # http://addictedtor.free.fr/graphiques/sources/source_137.R
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  # We don't use cor.test()$estimate, but result from cor()
  # That way, we have more flexibility in defining the "use" argument
  corr <- cor(x, y, use = use, method = method)

  txt <- format(c(corr, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  cor.cex <- cor.cex / strwidth(txt)

  test <- cor.test(x, y, alternative = alternative, method = method)

  star <- symnum(test$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cor.cex * abs(corr), ...)
  text(0.8, 0.8, as.character(star), cex = cor.cex, col = stars.col)

  invisible(test)
}

#' @export
#' @rdname panels
panel.cor <- panel_cor # Backward compatibility

#' @export
#' @rdname panels
panel_smooth <- panel.smooth
