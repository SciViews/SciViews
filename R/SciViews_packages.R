#' Give the list of SciViews::R packages and check for conflicts
#'
#' @description List required packages or conflicting functions. These functions
#'   are inspired by [tidyverse::tidyverse_packages()] and
#'   [tidyverse::tidyverse_conflicts()], but adapted to the SciViews::R context.
#'
#' @param ... Further topics to consider in `SciViews::R`. Currently, `"infer"`,
#'   `"model"`, `"explore"`, `"ml"`, `"ts"` or `"spatial"`.
#' @param all Should all packages be listed (`TRUE`) or only those that are
#'   attached to the search path (`FALSE`).
#' @param x A SciViews_conflicts object
#' @param startup Should the message be printed at startup?
#'
#' @return A list of packages for [SciViews::SciViews_packages()], or a
#'   **SciViews_conflicted** object with a [print()] method for
#'   [SciViews::SciViews_conflicts()].
#' @export
#'
#' @examples
#' # List of packages attached to the search path with SciViews::R
#' SciViews_packages()
#' # More complete list of packages used by SciViews::R
#' SciViews_packages(all = TRUE)
#' # Even more packages, by adding also 'model' and 'ml' topics
#' SciViews_packages("model", "ml", all = TRUE)
#' # Conflicts
#' SciViews_conflicts()
SciViews_packages <- function(..., all = FALSE) {
  if (isTRUE(all)) {
    pkgs <- list(SciViews = c('MASS', 'lattice', 'data.trame', 'broom', 'cli',
      'crayon', 'dbplyr', 'dplyr', 'dtplyr', 'forcats', 'googledrive',
      'googlesheets4', 'ggplot2', 'haven', 'hms', 'httr', 'jsonlite',
      'lubridate', 'magrittr', 'modelr', 'pillar', 'purrr', 'readr', 'readxl',
      'reprex', 'rlang', 'rstudioapi', 'rvest', 'stringr', 'tibble', 'tidyr',
      'xml2', 'tidyverse', 'collapse', 'fs', 'svMisc', 'svBase', 'svFlow',
      'data.io', 'chart', 'tabularise', 'SciViews'))
  } else {# Just the list of packages to attach to the search path
    pkgs <- list(SciViews = c('rlang', 'MASS', 'lattice', 'data.trame',
      'ggplot2', 'tibble', 'tidyr', 'dplyr', 'dtplyr', 'broom', 'forcats',
      'collapse', 'fs', 'svMisc', 'svBase', 'svFlow', 'data.io', 'chart',
      'tabularise', 'SciViews'))
  }

  # ... specifies the topics to use (= loading more packages)
  topics <- unlist(lapply(list(...), as.character))
  if (length(topics)) {
    all_pkgs <- SciViews_packages_topics(all = all)
    all_topics <- names(all_pkgs)
    # Check that topics are in the list, or issue an error message
    if (!all(topics %in% all_topics))
      stop("Unknown topic, you must give one or more of ",
        paste(all_topics, collapse = ", "))
    # Select only packages in the specific topics
    selected_pkgs <- all_pkgs[topics]
    # Add this to the SciViews topic
    pkgs <- c(pkgs, selected_pkgs)
  }
  pkgs
}

#' @export
#' @rdname SciViews_packages
SciViews_packages_topics <- function(all = FALSE) {
  if (isTRUE(all)) {
    pkgs <- list(
      infer   = c("distributional", "mvtnorm", "SuppDits", "inferit"),
      model   = c("broom", "MASS", "modelit"),
      explore = c("broom", "ca", "exploreit", "factoextra", "FactoMineR",
        "vegan"),
      ml      = c("class", "e1071", "ipred", "MASS", "mlbench", "mlearning",
        "nnet", "parsnip", "randomForest", "recipes", "ROCR", "rpart", "rsample"),
      ts      = c("boot", "pastecs"),
      # Note: maptools, rgdal, rgeos retired in 2023. sf should replace sp and
      # terra should replace raster. ggsn is archived and ggspatial provides
      # some of its functionnalities, see: https://r-spatial.org/book/sp-raster.html
      # So, we eliminate sp and raster from the list and replace ggsn by ggspatial
      spatial = c("ggspatial", "sf", "stars", "terra", "tmap")
    )
  } else {# Just the list of packages to attach to the search path
    pkgs <- list(
      infer   = c("distributional", "inferit"),
      model   = c("broom", "modelit"),
      explore = c("broom", "exploreit"),
      ml      = c("mlearning", "ROCR", "parsnip", "recipes", "rsample"),
      ts      = c("pastecs"),
      spatial = c("ggspatial", "sf", "stars", "terra", "tmap")
    )
  }
  pkgs
}


#' @export
#' @rdname SciViews_packages
SciViews_conflicts <- function(all = TRUE) {
  # This is adapted from tidyverse::tidyverse_conflicts()
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))
  conflicts <- purrr::keep(objs, ~length(.x) > 1)
  pkgs <- unique(unlist(SciViews_packages(all = all)))
  tidy_names <- paste0("package:", pkgs)
  conflicts <- purrr::keep(conflicts, ~any(.x %in% tidy_names))
  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)
  structure(conflict_funs, class = "SciViews_conflicts")
}

#' @export
#' @rdname SciViews_packages
#' @method print SciViews_conflicts
print.SciViews_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(SciViews_conflict_message(x))
}

# This is an unexported function from tidyverse
invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

# This is an unexported function from tidyverse
ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}

# This is an unexported function from tidyverse
confirm_conflict <- function(packages, name) {
  objs <-  purrr::keep(purrr::map(packages, ~get(name, pos = .)), is.function)
  if (length(objs) <= 1)
    return()
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()
  packages
}

# This is an modified version of an unexported function from tidyverse
SciViews_conflict_message <- function(x) {
  if (length(x) == 0)
    return("")
  header <- cli::rule(left = crayon::bold("Conflicts"),
    right = "SciViews_conflicts()")
  pkgs <- purrr::map(x, ~gsub("^package:", "", .))
  others <- purrr::map(pkgs, `[`, -1)
  other_calls <- purrr::map2_chr(others, names(others),
    ~paste0(crayon::blue(.x), "::", .y, "()", collapse = ", "))
  winner <- purrr::map_chr(pkgs, 1)
  funs <- format(paste0(crayon::blue(winner), "::",
    crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(crayon::red(cli::symbol$cross), " ", funs,
    " masks ", other_calls, collapse = "\n")
  paste0(header, "\n", bullets)
}
