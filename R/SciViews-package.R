#' 'SciViews::R' Dialect for Data Processing and Visualization
#'
#' The `SciViews::R` dialect is base R + tidyverse + a series of additional
#' SciViews packages like data.io, svBase, svFlow, tabularise or chart.

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
#' @docType package
#' @name SciViews-package

## usethis namespace: start
#' @import stats
#' @import graphics
#' @importFrom utils packageVersion stack
#' @importFrom grDevices colorRampPalette chull hsv rainbow
#' @importFrom ellipse ellipse plotcorr
#' @importFrom crayon num_colors num_ansi_colors white black red blue green bold col_align col_nchar
#' @importFrom cli cat_bullet cat_line col_blue rule style_bold symbol
#' @importFrom rstudioapi getThemeInfo hasFun isAvailable
#' @importFrom purrr compact imap keep map map_chr map2_chr set_names
#' @importFrom svBase as_dtf as_dtt as_dtbl as_dtx is_dtf is_dtt is_dtbl
#' @import tabularise
## usethis namespace: end
NULL
