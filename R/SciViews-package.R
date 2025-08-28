#' 'SciViews::R' - Data Processing and Visualization R Dialect
#'
#' The `SciViews::R` dialect is base R + tidyverse + a series of additional
#' SciViews packages like data.io, svBase, svFlow, tabularise or chart.

#' @section Important functions:
#'
#'- [SciViews::R()] for loading the `SciViews::R`` packages,
#'
#' - [SciViews::pcomp()] for a PCA analysis (unifying various methods),
#'
#' - [SciViews::correlation()] to calculate and plot a correlation matrix,
#'
#'- [SciViews::panel_reg()] and others to plot panels in `pairs` or `coplot` graphs,
#'
#'- [SciViews::panel_boxplot()] and others for univariate panels in `pairs` plots.
#'
#' - [SciViews::rwb_colors()] and others to generate color palettes.
#'
#' - [SciViews::enum()] to enumerate items in a vector,
#'
#' - [SciViews::timing()] to determine the time required to run an R expression,
#'
#' - [SciViews::nr()] and co as convenient shorthand to columns and rows,
#'
#' - [SciViews::ln()] and others for natural logarithm.
#'
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import stats
#' @import graphics
#' @importFrom utils getFromNamespace help packageVersion stack
#' @importFrom grDevices colorRampPalette chull hsv rainbow
#' @importFrom ellipse ellipse plotcorr
#' @importFrom crayon num_colors num_ansi_colors white black red blue green bold col_align col_nchar
#' @importFrom cli cat_bullet cat_line col_blue rule style_bold symbol
#' @importFrom rlang global_entrace local_use_cli on_load run_on_load
#' @importFrom rstudioapi getThemeInfo hasFun isAvailable
#' @importFrom purrr compact imap keep map map_chr map2_chr set_names
#' @importFrom svBase as_dtf as_dtt as_dtbl as_dtrm as_dtx is_dtf is_dtt is_dtbl is_dtrm
#' @importFrom data.trame data.trame
#' @import tabularise
#' @importFrom httr2 req_body_json req_headers req_perform request resp_body_json
#' @importFrom roxygen2 roxy_tag tag_markdown
## usethis namespace: end
NULL
