#' Logarithmic and exponential functions
#'
#' @description  `ln()` computes natural logarithm, `lg()` computes base 10 logarithm, and
#' `lb()` computes binary (base 2) logarithm.
#'
#' `ln1p()` and `lg1p()` computes `ln(x + 1)` and `lg(x + 1)` accurately also
#' for `|x| << 1`.
#'
#' `E` is the Euler constant and is equal to `exp(1)`.
#'
#' @param x A numeric or complex vector.
#' @details Those functions are synonyms of `log()`, `log10()`, `log2()`,
#' `log1p()` for those who prefer the shorter notation. Beginners sometimes
#' make confusion between `log()` and `log10()`. Using `ln()` for natural
#' logarithms instead of `log()` eliminates this confusion. `E` is provided for
#' convenience as `exp(1)`, although the use of `exp()` is usually familiar
#' enough to everyone.
#' @export
#' @seealso [log()]
#' @keywords math
#' @concept logarithm and exponential
#' @examples
#' ln(exp(3))              # Same as log(exp(3))
#' lg(10^3)                # Same as log10(10^3)
#' lb(1:3)                 # Wrapper for log2()
#'
#' ln1p(c(0, 1, 10, 100))  # Wrapper for log1p()
#' lg1p(c(0, 1, 10, 100))  # log10(x + 1), but optimized for x << 1
#'
#' E^4                     # Similar to exp(4), but different calculation!
ln <- function(x) log(x)

#' @export
#' @rdname ln
lg <- function(x) log10(x)

#' @export
#' @rdname ln
lb <- function(x) log2(x)

#' @export
#' @rdname ln
ln1p <- function(x) log1p(x)

#' @export
#' @rdname ln
lg1p <- function(x) log1p(x) / log(10)

#' @export
#' @rdname ln
E <- exp(1)
