#' SciViews - Main package
#'
#' The SciViews package provides various functions to install the `SciViews::R`
#' dialect. It also provides additional function to supplement base, recommended
#' and tidyverse packages.
#'
#' @section Important functions:
#'
#'- [SciViews::R()] for loading the SciViews::R packages,
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
#' - [timing()] to determine the time riquired to run an R expression,
#'
#' - [nr()] and co as convenient shorthands to columns and rows,
#'
#' - [ln()] and others for natural logarithm.
#'
#' @docType package
#' @name SciViews-package
#'
#' @import stats
#' @import graphics
#' @importFrom grDevices colorRampPalette chull hsv rainbow
#' @importFrom ellipse ellipse plotcorr
#' @import cli
#' @import crayon
#' @import rstudioapi
#'
#' @import svMisc
#' @import tidyverse
NULL
