#' @details
#' The `Sciviews::R` dialect is base R + tidyverse + a series of additional SciViews packages like 'data.io', 'flow' or 'chart'.

#' @section Important functions:
#'
#'- [SciViews::R()] for loading the `SciViews::R`` packages,
#'
#' - [pcomp()] for a PCA analysis (unifying various methods),
#'
#' - [correlation()] to calculate and plot a correlation matrix,
#'
#'- [panel_reg()] and others to plot panels in `pairs` or `coplot` graphs,
#'
#'- [panel_boxplot()] and others for univariate panels in `pairs` plots.
#'
#' - [rwb_colors()] and others to generate color palettes.
#'
#' - [enum()] to enumerate items in a vector,
#'
#' - [timing()] to determine the time required to run an R expression,
#'
#' - [nr()] and co as convenient shorthand to columns and rows,
#'
#' - [ln()] and others for natural logarithm.
#'
#' @keywords internal
"_PACKAGE"

#' @import stats
#' @import graphics
#' @importFrom grDevices colorRampPalette chull hsv rainbow
#' @importFrom ellipse ellipse plotcorr
#' @import cli
#' @import crayon
#' @import rstudioapi
#'
#' @import tidyverse
#' @import svMisc
#' @import flow
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
