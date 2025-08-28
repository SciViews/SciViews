#' Configure \R for the SciViews::R dialect
#'
#' Load required packages like data.table, collapse, ggplot2, dplyr, svMisc, ...
#' to get a fully functional `SciViews::R` dialect environment.
#'
#' @param ... Further topics to include to configure \R (load more packages).
#'   Currently, `"infer"`, `"model"`, `"explore"`, `"ml"`, `"ts"` or `"spatial"`
#' @param lang What is the default natural language to use, e.g., `"en"` or
#'   `"fr"`, with uppercase versions `"EN"` or `"FR"` convert even more strings,
#'   for instance, [data.io::read()] does not convert factor levels in the
#'   corresponding language for supported data sets unless the uppercase version
#'   is specified. If `NULL` (by default), current configuration is not changed.
#' @param dtx Which dtx object is to be used be default? `"dtt"` or
#'   `"data.table"` for data.table, `"dtf"` or `"data.frame"` for data.frame,
#'   `"dtbl"`, `"tibble"` or `"tbl_df"` for tibble's tbl_df, the name of a
#'   function to use to convert a data.frame object, or `NULL` (by default) to
#'   keep current settings.
#' @param threads.percent The percentage of threads to use for \{data.table\} and
#'   \{collapse\} parallel code (number of threads depend on how many are
#'   available, and the value is rounded towards zero).
#' @param silent If `TRUE` (by default), no report is printed about loaded
#'   packages and conflicts.
#' @param warn If `TRUE` (by default), warnings are issued when there is a
#' partial matching of function argument, of an attribute, or of `$`
#' (corresponding to the R options `warnPartialMatchArgs`,
#' `warnPartialMatchAttr` and `warnPartialMatchDollar` set to `TRUE`).
#' @param global_entrace Should the [rlang::global_entrace()] be activated for
#'   certain classes of condition messages? By default, for `"error"`. Specify
#'   `NULL` if you do not want to use this feature.
#' @param x An object to print.
#' @export
#' @name SciViews_R
#' @note Use `SciViews::R` instruction in the beginning of an R script, or in
#' the setup or first chunk of an R Markdown/Notebook to ensure the SciViews::R
#' dialect is correctly installed. The report indicating attached packages and
#' conflicts is largely inspired by the corresponding `tidyverse` code,
#' written by Hadley Wickham.
#' @seealso [library()], [utils::install.packages()]
#' @keywords utilities
#' @concept packages loading
#' @examples
#' \dontrun{
#' SciViews::R
#' }
R <- structure(function(..., lang = NULL, dtx = NULL, threads.percent = 75,
silent = TRUE, warn = TRUE, global_entrace = "error") {

  if (!is.null(global_entrace) && length(global_entrace)) {
    # Avoid calling twice global_entrace() with the same argument
    current_entrace <- .get_temp(".current_global_entrace", default = "")
    if (!identical(global_entrace, current_entrace)) {
      global_entrace(class = global_entrace)
      .assign_temp(".current_global_entrace", global_entrace)
    }
  }

  # Configure the system to use a certain number of threads in data.table and
  # collapse, and mask all functions in collapse
  data.table::setDTthreads(percent = threads.percent)
  nthreads <- data.table::getDTthreads()
  options(collapse_nthreads = nthreads)
  options(collapse_na.rm = TRUE) # Default value for na.rm collapse functions
  # No, deal with it differently, because it does not work if {collapse} was
  # already loaded previously without this option!
  #options(collapse_mask = "all") # Mask functions from base like mean(), sd()

  pkgs <- SciViews_packages(..., all = FALSE)
  # Flatten the list, and eliminate duplicates
  pkgs <- unique(unlist(pkgs))

  crayon::num_colors(TRUE)
  old_search_length <- length(search())
  loaded <- sapply(pkgs, silent_library)
  failed_pkgs <- pkgs[!loaded]
  if (length(failed_pkgs) == 1L) {
    stop(gettextf("problem while loading the package '%s';\n\tRun `Install()` to make sure everything is correctly installed!",
      failed_pkgs))
  } else if (length(failed_pkgs) > 1L) {
    stop(gettextf("problem while loading the following packages: '%s';\n\tRun `Install()` to make sure everything is correctly installed!",
      paste(failed_pkgs, collapse = "', '")))
  }

  if (!is.null(lang)) {
    if (length(lang) != 1 || !is.character(lang))
      stop("You must provide a single character string for 'lang='.")
    options(SciViews_lang = lang)
  }
  # Message if default language set
  cur_lang <- getOption("SciViews_lang")
  if (!is.null(cur_lang) && !isTRUE(silent))
    cli::cat_bullet(cli::col_blue("Default language: "),
      cli::style_bold(cur_lang), bullet = "tick", bullet_col = "green")

  if (!is.null(dtx)) {# Change the default dtx object for {svBase}
    dtx <- as.character(dtx)[1]
    fun <- switch(dtx,
      dtrm = as_dtrm,
      data.trame = as_dtrm,
      dtf = as_dtf,
      data.frame = as_dtf,
      dtt = as_dtt,
      data.table = as_dtt,
      dtbl = as_dtbl,
      tibble = as_dtbl,
      tbl_df = as_dtbl,
      get0(dtx, mode = 'function')
    )
    if (is.null(fun))
      stop(gettextf("Function %s not found", dtx))
    options(SciViews.as_dtx = fun)
  }
  # Check which kind of object I got by using as_dtx() on a toy data.frame
  test <- as_dtx(data.frame(x = 1))
  if (is_dtrm(test)) {
    dtx_class <- "data.trame"
  } else if (is_dtt(test)) {
    dtx_class <- "data.table"
  } else if (is_dtbl(test)) {
    dtx_class <- "tibble"
  } else if (is_dtf(test)) {
    dtx_class <- "data.frame"
  } else {
    dtx_class <- class(test)[1]
  }

  if (!isTRUE(silent) && length(search()) > old_search_length) {
    # Message about the dtx object by default
    cli::cat_bullet(cli::col_blue("Default data frame object (dtx): "),
      cli::style_bold(dtx_class), bullet = "tick", bullet_col = "green")

    packages_versions(strip.last = old_search_length)

    x <- SciViews_conflicts(all = FALSE)
    print(x)

    # Message about the dtx object by default
    #cli::cat_rule("Default data frame object (dtx)", right = dtx_class)
  }

  if (isTRUE(warn))
    options(warnPartialMatchArgs = TRUE, warnPartialMatchAttr = TRUE,
      warnPartialMatchDollar = TRUE)

    invisible(list(pkgs = pkgs, dtx_class = dtx_class))
}, class = c("SciViews_R", "function"))

#' @rdname SciViews_R
#' @export
print.SciViews_R <- function(x, ...) {
  x()
  invisible(x)
}

# Silently load packages and return info if the package was loaded
# Note: currently, accepts only one pkg at a time
silent_library <- function(pkg) {
  ex <- switch(pkg,
    ggplot2 = "aes_",
    tibble  = c("data_frame_", "lst_", "tibble_"),
    dplyr   = c("add_count_", "add_tally_", "arrange_", "count_", "distinct_",
      "do_", "filter_", "funs_", "group_by_", "group_indices_", "mutate_",
      "mutate_each_", "rename_", "rename_vars_", "select_", "select_vars_",
      "slice_", "summarise_", "summarise_each_", "summarize_",
      "summarize_each_", "tally_", "transmute_"),
    tidyr    = c("complete_", "crossing_", "drop_na_", "expand_", "extract_",
      "fill_", "gather_", "nest_", "nesting_", "separate_", "separate_rows_",
      "spread_", "unite_", "unnest_"),
    NULL)

  res <- try(suppressPackageStartupMessages(
    library(pkg, character.only = TRUE, warn.conflicts = FALSE,
      exclude = ex)), silent = TRUE)

  if (inherits(res, "try-error")) {
    # Record the package for easier Install()
    to_install <- .get_temp('.packages_to_install', default = character(0))
    to_install <- unique(c(pkg, to_install))
    .assign_temp('.packages_to_install', to_install, replace.existing = TRUE)
    #warning("unable to load package '", pkg, "'; is it correctly installed?")
    return(invisible(FALSE))
  }
  invisible(TRUE)
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
