#' Logarithms.
#'
#' To avoid confusion using the default `log()` function, which is natural
#' logarithm, but spells out like base 10 logarithm in the mind of some
#' beginneRs, we define `ln()` and `ln1p()` as wrappers for `log()`` with
#' default `base = exp(1)` argument and for `log1p()`, respectively.
#' For similar reasons, `lg()` is a wrapper of `log10()` (there is no possible
#' confusion here, but 'lg' is another common notation for base 10 logarithm).
#' `lg1p()` is a convenient way to use the optimized code to calculate the
#' logarithm of x + 1, but returning the result in base 10 logarithm. `E` is the
#' Euler constant and is provided for convenience as `exp(1)`. Finally `lb()` is
#' a synonym of `log2()`.
#'
#' @param x A numeric or complex vector.
#' @export
#' @seealso [log()]
#' @keywords math
#' @concept logarithms and exponentials
#' @examples
#' ln(exp(3))              # Same as log(exp(3))
#' ln1p(c(0, 1, 10, 100))  # Wrapper for log1p()
#' lg(10^3)                # Same as log10(10^3)
#' lg1p(c(0, 1, 10, 100))  # log10(x + 1), but optimized for x << 1
#' E^4                     # Similar to exp(4), but different calculation!
# Note: exp(4) is to be preferred to E^4, if possible!
#' lb(1:3)                 # Wrapper for log2()
ln <- function(x) log(x)

#' @export
#' @rdname ln
ln1p <- log1p

#' @export
#' @rdname ln
lg <- log10

#' @export
#' @rdname ln
lg1p <- function(x) log1p(x) / log(10)

#' @export
#' @rdname ln
E <- exp(1)

#' @export
#' @rdname ln
lb <- log2
