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
#' @importFrom utils packageVersion stack
#' @importFrom grDevices colorRampPalette chull hsv rainbow
#' @importFrom ellipse ellipse plotcorr
#' @importFrom crayon num_colors num_ansi_colors white black red blue green bold col_align col_nchar
#' @importFrom cli cat_line rule symbol
#' @importFrom rstudioapi getThemeInfo hasFun isAvailable
#' @importFrom purrr compact imap keep map map_chr map2_chr set_names
#' @importFrom svBase as_dtf as_dtt as_dtbl as_dtx is_dtf is_dtt is_dtbl
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
