# SciViews 1.6.0

-   Packages {tabularise} and {inferit} added.

-   Default for `silent=` argument of SciViews::R is now `TRUE`

-   The parameter `threads.percent=` argument is added to `SciViews::R`. It defines the percentage of threads that both {data.table} and {collapse} can use for their parallelized code.

# SciViews 1.5.0

-   Topics are now implemented in `SciViews::R`. One can specify, for instance, `SciViews::R("model")` to load more packages related to modeling with regressions. Current topics are 'infer', 'model', 'explore', 'ml', 'ts' and 'spatial'. It is possible to specify more than one topic.

-   The new function `sciviews_packages_topics()` lists all packages required for each topic.

-   `SciViews::R` has now a new argument `lang=` to specify the default language to use. Hence, `SciViews::R(lang = "fr")` will set French as the default language used by functions like `data.io::read()`, or for plot axes labels or title.

# SciViews 1.4.0

-   {tidyverse} is not loaded any more, but a selected series of tidyverse packages useful for `SciViews::R`.

-   `SciViews::R` has now an argument to set which is the default data frame object to use (data.frame, tibble, or data.table with `as_dtx=`). The default value is data.table. In all cases, a message is printed to clearly indicate which is the default.

-   Startup messages with `SciViews::R` are now more complete regarding functions conflicts (it does not use `tidyverse_conflicts()` any more).

# SciViews 1.3.0

-   The {svBase}, {data.table}, {dtplyr} and {collapse} packages are now included in the list of the SciViews::R packages. With {svBase} the preferred data frame object is now data.table, and since {dtplyr} is loaded, it is used for {dplyr} verbs by default.

# SciViews 1.2.0

-   Change dependency to {svFlow} instead of {flow} after that package was renamed.

# SciViews 1.1.2

-   Restrict {cli} and {crayon} imports to avoid a conflict with `num_ansi_colors()` that is defined in both packages when called from `ansi::num_colors()`.

-   {rmarkdown} dependency added to suggests field for the vignettes.

# SciViews 1.1.1

-   `panel_smooth()` is reworked to avoid a bug in the documentation. It still calls `graphics::panel.smooth()` internally.

-   A 'pkgdown' web site is added.

# SciViews 1.1.0

-   'flow', 'chart', and 'data.io' are now part of the packages automatically loaded by `SciViews::R`.

# SciViews 1.0-0

-   `SciViews::R` is the instruction to use to load all required packages to install a complete SciViews::R dialect (R base + tidyverse + ...).

# SciViews 0.9-14

-   Better definition of `lg()`, `lb()` and `ln1p()`.

# SciViews 0.9-13

-   'correlation' objects are now 'Correlation' to avoid clash with 'correlation' objects from 'nlme' package.

-   Doc rewritten in Roxygen2 and R Markdown (keeping only pca vignette). More strict `importFrom()` in particular from 'ellipse' package.

-   Elimination of files.R, character.R & graphics.R (experimental code that never reached CRAN).

-   Code rewritten to match tidyverse style guide. Functions that are not snake case like `rwb.colors()` or `panel.hist()` now are seconded by they equivalent `rwb_colors()` or `panel_hist()`.

# SciViews 0.9-12

-   Switch from R-Forge to GitHub <https://github.com/SciViews/SciViews>. CI added.

# SciViews 0.9-12

-   `p()` is renamed `p0()` to avoid a clash with `p()` in the 'ascii' package.

-   `rx` objects are renamed `regex` and `perl` objects are renamed `pcre`.

-   `fileSymlink()` is renamed `fileSymLink()` for correct camel case support.

# SciViews 0.9-11

-   `path` object is renamed `filePath` to avoid a clash with a path object in 'grid' package. Related function `path()`, `is.path()`, `as.path()` are renamed accordingly `filePath()`, `is.filePath()` and `as.filePath()`. The `print()` method is also adapted.

-   `warnAssignWithEqualSign`, `warnPartialMatchArgs`, `warnPartialMatchAttr` and `warnPartialMatchDollar` in `options()` are now initialized to `FALSE` if they are not defined yet there.

# SciViews 0.9-10

-   Import from `data.table` is eliminated. For now, `@:=` is the same as `@<-`.

-   Temporary objects are now saved in `SciViews:TempEnv` instead of `TempEnv`.

# SciViews 0.9-9

-   Improvements to activate warnings regarding possible R traps: if `warnPartialMatchArgs`, `warnPartialMatchAttr`, or `warnPartialMatchDollar` options are not defined yet, they are set to `TRUE` when the packages loads.

-   For a similar purpose, we would like to avoid using `=` in place of `<-` for assignation. So, the `=` function is redefined to display a warning when it is used and when `warnAssignWithEqualSign` option is set to `TRUE` (by default). The warning message also suggests it may be `==` erroneously written `=`.

-   Many functions are added with more coherent names for graphics.

# SciViews 0.9-8

-   Several changes in character.R.

-   Added the `rwg.colors()` function.

# SciViews 0.9-7

-   Small corrections in man pages.

-   Partial argument matching for `all(.names)` in `names()`. Fixed.

# SciViews 0.9-6

-   Added further (misc) functions.

-   New syntax using `x@attr` for attributes, plus `:=` for replacement by reference inspired from `data.table` package, which SciViews now imports too.

# SciViews 0.9-5

-   Added functions to homogenize function names for strings and files manipulations.

# SciViews 0.9-4

-   The gamma argument in `hsv()` function disappears in R 2.14.0. As a consequence, the same gamma argument is dropped from `rwb.colors()`, `ryg.colors()` and `cwm.colors()`.

# SciViews 0.9-3

-   A bug in `svd.pca()` subfunction of `pcomp.default()` was corrected.

# SciViews 0.9-2

-   Slight style refactoring of R code and man pages.

# SciViews 0.9-1

-   Added `lb()` function as a synonym of `log2()`.

# SciViews 0.9-0

-   This is the first version on R-forge. There used to be a SciViews **bundle** that contained 'svMisc', 'svSocket', 'svGUI', ..., but bundles are obsolete now. The new 'SciViews' package plays a similar role as the bundle.
