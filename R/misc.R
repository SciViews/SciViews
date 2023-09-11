#' Enumerate items in an object
#'
#' `enum()` creates a vector of integers from 1 to length of the object (it
#' enumerates items in the object), except if the object is empty. It is
#' particularly useful in the `for(i in enum(object))` construct.
#'
#' @param x Any object.
#' @note The pattern `for(i in 1:length(object))` is often found, but it fails
#' in case `length(object) == 0`! `enum()` is indeed a synonym of `seq_along()`,
#' but the later one is less  expressive in the context.
#' @export
#' @seealso [seq_along()]
#' @examples
#' enum(letters)
#' enum(numeric(0))
#' # Compare with:
#' 1:length(numeric(0))
#' enum(NULL)
#' letters5 <- letters[1:5]
#' for (i in enum(letters5)) cat("letter", i, "=", letters5[i], "\n")
enum <- function(x) seq_along(x)


#' Convenience functions for rows or columns manipulations
#'
#' @description `nr()` and `nc()` are synonyms of the ugly `NROW()` or `NCOL()`
#' that get the number of row and columns in a matrix or data frame, but also in
#' a vector (they return a value even if the `dim` attribute of the object is
#' not set, on the contrary to `nrow()`or `ncol()`).
#'
#' `ROWS` and `COLS` are constants that makes call to `apply()` more expressive.
#' `ROWS = 1L` and `COLS = 2L`.
#'
#' @param x Any object.
#' @export
#' @seealso [nrow()]
#' @examples
#' mm <- matrix(1:6, nrow = 3)
#' nr(mm)
#' nc(mm)
#'
#' vv <- 1:6
#' nr(vv)
#' nc(vv)
#'
#' # ROWS and COLS constants used with apply()
#' apply(mm, ROWS, mean) # Idem apply(mm, 1, mean)
#' apply(mm, COLS, mean) # Idem apply(mm, 2, mean)
nr <- NROW

#' @export
#' @rdname nr
nc <- NCOL

#' @export
#' @rdname nr
ROWS <- 1L

#' @export
#' @rdname nr
COLS <- 2L


#' Timing of R expressions
#'
#' Similar to `system.time()` but returns a more convenient 'difftime' object
#' with the overall timing (details are stored in the `details` attribute).
#'
#' @param expr Valid \R expression to be timed. If missing, [proc.time()] is
#' used instead and the function returns the time the currently running \R
#' process has already taken.
#' @param gc.first Logical - should a garbage collection be performed immediately
#' before the timing? Default is `TRUE`.
#' @export
#' @seealso [system.time()], [proc.time()]
#' @examples
#' test <- timing(Sys.sleep(0.5))
#' test
#' attr(test, "details")
timing <- function(expr, gc.first = TRUE) {
  if (missing(expr)) {
    res <- proc.time()
  } else {
    res <- system.time(expr, gcFirst = gc.first)
  }
  details <- as.difftime(res, units = "secs")
  res <- as.difftime(res["elapsed"], units = "secs")
  res <- details["elapsed"]
  attr(res, "details") <- details
  res
}

# To do later... ----------------------------------------------------------

# For non S4 objects, reuse @ to set attributes
# After all, they are, indeed, attributes!
# Note that we force exact match (same behaviour as @ used for S4 objects)
# Benchmark shows that my version is, at least 10x slower than the original `@`
# or `@<-` for S4 objects => should I use it or not?!
#Loc <- setClass("Loc", slots = c(lat = "numeric", long = "numeric"))
#loc <- Loc(lat = 0, long = 0)
#at <- base::`@`
#`at<-` <- base::`@<-`
#identical(loc@lat, at(loc, lat))
#identical(loc@lat <- 1, at(loc, lat) <- 1)
#mbench <- microbenchmark::microbenchmark
#mbench(loc@lat, at(loc, lat), loc@lat <- 1, at(loc, lat) <- 1)
#`@` <- function(object, name) {
#  arg <- substitute(name)
#  if (is.name(arg)) name <- as.character(arg)
#  if (isS4(object)) slot(object, name) else attr(object, name, exact = TRUE)
#}
#
#`@<-` <- function(x, which, value){
#  arg <- substitute(which)
#  if (is.name(arg)) which <- as.character(arg)
#  if (isS4(x)) {
#    `slot<-`(x, which, check = TRUE, value)
#  } else {
#    `attr<-`(x, which, value)
#  }
#}

# classes <- function(x) {
#   # Special case for a missing argument
#   X <- substitute(x)
#   res <- try(missing(X), silent = TRUE)
#   if (isTRUE(res)) return(c("missing", "ANY"))
#   # Return the class of an object plus atomic/recursive and ANY
#   res <- class(x)
#   # Special case for NULL
#   if (res == "NULL") return(c("NULL", "atomic", "ANY"))
#   # Special case for name which is neither atomic, nor recursive
#   if (res == "name") return(c("name", "symbol", "language", "ANY"))
#   # Is this a recursive or atomic object?
#   if (is.recursive(x)) {
#     # Is this a language object?
#     if (is.language(x)) c(res, "language", "recursive", "ANY") else
#       c(res, "recursive", "ANY")
#   } else if (is.atomic(x)) {
#     c(res, "atomic", "ANY")
#   } else c(res, "ANY")
# }


# Warn when using = instead of <- for assignation...
# if option warnAssignWithEqualSign is TRUE
# NOTE: names(x) <- "a" assigns "a" to `names(x)` => this is wrong!
#`=` <- function(x, value) {
#  if (isTRUE(getOption("warnAssignWithEqualSign")))
#    warning("Use <- instead of = for assignation, or use == for equality test")
#  assign(deparse(substitute(x)), value, envir = parent.frame())
#}

#`%else%` <- function(test, expr) if (test) invisible() else expr
# Useful to write shorter code in something like:
#test %else% break
#test %else% stop(msg)
#test %else% return(res)

# How to simplify the use of if() by limiting possible special cases?
# use of any() and all() is there to cope with this, but still:
# 1) any(NA) => NA, unless any(NA, na.rm = TRUE) => FALSE
# 2) any(NULL) & any(logical(0)) => FALSE => OK
# We solve this by defining any.() and all.()
#any. <- function(..., na.rm = TRUE) any(..., na.rm = na.rm)
#all. <- function(..., na.rm = TRUE) all(..., na.rm = na.rm)
#one <- function(x, na.rm = FALSE) UseMethod("one")
#one.default <- function(x, na.rm = FALSE) {
#  if (isTRUE(na.rm)) x <- na.omit(x)
#  identical(TRUE, as.logical(x))
#}
#one. <- function(x, na.rm = TRUE) one(x, na.rm = na.rm)
# TODO: other xxx. functions for those using na.rm = FALSE
# like mean, median, sd, var, quantile, fivenum, ...

#`%is%` <- function(x, class) inherits(x,class)
