# ln(x) and ln1p(x) are wrappers for log(x) and log1p(x) to avoid confusion
# with log10(x) that some beginneRs do, thinking that log(x) is logarithm in
# base 10! lg(x) is a wrapper for log10(x) for the same reason.
# lg1p(x) is the same as log1p() but it returns its result in base 10 log
# 'e' is a useful constant and is equal to exp(1)
ln <- function (x) log(x)

ln1p <- function (x) log1p(x)

lg <- function (x) log10(x)

lg1p <- function (x) log1p(x) / log(10)

e <- exp(1)
