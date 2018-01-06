.onLoad <- function(lib, pkg) { # nocov start
  # If corresponding options are not defined yet, specify them to FALSE
  # by default, but make them explicitly available in options()
  if (!length(getOption("warnAssignWithEqualSign")))
    options(warnAssignWithEqualSign = FALSE)
  if (!length(getOption("warnPartialMatchArgs")))
    options(warnPartialMatchArgs = FALSE)
  if (!length(getOption("warnPartialMatchAttr")))
    options(warnPartialMatchAttr = FALSE)
  if (!length(getOption("warnPartialMatchDollar")))
    options(warnPartialMatchDollar = FALSE)
} # nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

.packageName <- "SciViews" # nocov

# Code borrowed from svMisc, to avoid a dependency!
.TempEnv_ <- function() {
  srch <- search()
  pos <-  match("SciViews:TempEnv", srch)
  if (is.na(pos)) {# Must create it
    pos <- length(srch) - 1
    `SciViews:TempEnv` <- list()
    Attach <- function(...) get("attach", mode = "function")(...)
    Attach(`SciViews:TempEnv`, pos = pos)
  }
	pos.to.env(pos)
}

.assignTemp <- function(x, value, replace.existing = TRUE) {
  TempEnv <- .TempEnv_()
  if (isTRUE(replace.existing) || !exists(x, envir = TempEnv, mode = "any",
    inherits = FALSE)) {
    assign(x, value, envir = TempEnv)
  }
}

# A copy of the unexported stats:::.check_vars_numeric
.check_vars_numeric <- function(mf) {
  mt <- attr(mf, "terms")
  mterms <- attr(mt, "factors")
  mterms <- rownames(mterms)[apply(mterms, 1L, function(x) any(x > 0L))]
  any(sapply(mterms, function(x) is.factor(mf[, x]) || !is.numeric(mf[, x])))
}
