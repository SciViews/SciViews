#' Configure \R for the SciViews::R dialect
#'
#' Load required packages like tidyverse, ggplot2, dplyr, svMisc, ... to get a
#' fully functional `SciViews::R` dialect environment.
#'
#' @param ... Further parameters to configure \R (not used yet).
#' @param silent If `TRUE`, no report is printed about loaded packages and
#' conflicts.
#' @param x An object to print.
#' @export
#' @name SciViews_R
#' @note Use `SciViews::R` instruction in the beginning of an R script, or in
#' the setup or first chunk of an R Markdown/Notebook to ensure the SciViews::R
#' dialect is correctly installed. The report indicating attached packages and
#' conflicts is largely inspired by the corresponding `tidyverse` code,
#' written by Hadley Wickham.
#' @seealso [package()], [Install()]
#' @keywords utilities
#' @concept packages loading
#' @examples
#' \dontrun{
#' SciViews::R
#' }
R <- structure(function(..., silent = FALSE) {
  pkgs <- c('MASS', 'lattice', 'data.table', 'tidyverse', 'svMisc', 'svBase',
    'dtplyr', 'svFlow', 'data.io', 'chart', 'collapse', 'SciViews')

  # TODO: deal with further arguments to configure specialized sub-systems

  crayon::num_colors(TRUE)
  old_search_length <- length(search())
  lapply(pkgs, silent_library)

  if (!isTRUE(silent) && length(search()) > old_search_length) {
    packages_versions(strip.last = old_search_length)

    x <- tidyverse_conflicts()
    print(x) #msg(conflict_message(x), startup = TRUE)
  }

  invisible(pkgs)
}, class = c("SciViews_R", "function"))

#' @rdname SciViews_R
#' @export
print.SciViews_R <- function(x, ...) {
  x()
  invisible(x)
}

# Silently load packages and issue an error if not loaded
# TODO: use the Install() mechanism...
silent_library <- function(pkg) {
  res <- try(
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    # Record the package for easier Install()
    to_install <- .get_temp('.packages_to_install', default = character(0))
    to_install <- unique(c(pkg, to_install))
    .assign_temp('.packages_to_install', to_install, replace.existing = TRUE)
    stop("problem while loading package '", pkg,
      "'; run `Install()` to make sure it is corrently installed!",
      call. = FALSE)
  }
}

# This is an unexported function from tidyverse
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable())
    return(x)

  if (!rstudioapi::hasFun("getThemeInfo"))
    return(x)

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

# This is an unexported function from tidyverse
msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("SciViews.quiet")))
      packageStartupMessage(text_col(...))
  } else {
    message(text_col(...))
  }
}

# This is an unexported version from tidyverse
package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3)
    version[4:length(version)] <-
      crayon::red(as.character(version[4:length(version)]))

  paste0(version, collapse = ".")
}

# TODO: use my own conflicts print message instead
# This is a modified version of tidyverse::tidyverse_conflict_message()
#conflict_message <- function(x) {
#  if (length(x) == 0) return("")
#
#  header <- cli::rule(
#    left = crayon::bold("Conflicts"),
#    right = "use pkg::fun() to access fun directly"
#  )
#
#  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
#  others <- pkgs %>% purrr::map(`[`, -1)
#  other_calls <- purrr::map2_chr(
#    others, names(others),
#    ~ paste0(crayon::blue(.x), "::", .y, "()", collapse = ", ")
#  )
#
#  winner <- pkgs %>% purrr::map_chr(1)
#  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
#  bullets <- paste0(
#    crayon::red(cli::symbol$cross), " ", funs,
#    " masks ", other_calls,
#    collapse = "\n"
#  )
#
#  paste0(header, "\n", bullets)
#}

# This is a modified version of tidyverse::tidyverse_attach() keeping only
# the information message about packages/versions loaded
packages_versions <- function(pkgs = NULL, strip.last = 0, filter = TRUE) {
  if (is.null(pkgs))
    pkgs <- search()
  if (strip.last > 0) {
    keep <- length(pkgs) - strip.last
    if (keep < 1)
      return("")
    # If this is the search path, .GlobalEnv remains in the first position, so,
    # need to shift selection by one place in this case
    pkgs <- pkgs[(1:keep) + as.numeric(isTRUE(filter))]
  }
  if (isTRUE(filter)) {
    pkgs <- pkgs[grepl("^package:", pkgs)]
    pkgs <- substring(pkgs, 9)
  }
  if (!length(pkgs))
    return("")

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("SciViews::R ", package_version('SciViews'))
    ),
    startup = TRUE
  )

  versions <- vapply(pkgs, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(pkgs)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  invisible(NULL)
}

# Functions we need to import:
# is.null -> is_null
# inherits -> `%is%`
# env(), child_env() and new_environment() are too slow in rlang but should replace new.env()
# is_function <- is.function
# is_primitive <- is.primitive
# do_call <- do.call # Note: rlang uses invoke(), but 100x slower!
# as_chr <- as.character # Note that as_character() and as_string() do something else in rlang!
# wrapr::qc() is quoting-concatenating function, see qc(a, b, c)!
#is_chr <- is.character
#is_env <- is.environment
#stop_if_not <- stopifnot
#capture_output <- capture.output
