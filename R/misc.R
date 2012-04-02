## A series of functions defined or redefined for a simpler or better use of R

# is.wholenumber(), see ?as.integer => define isWholeInt?

## This should be nice:
## Define a valid method to be applied to S3 objects to make sure they are
## correct
valid <- function (object, ...)
	UseMethod("valid")
	
valid.default <- function (object, ...)
	return(object)

## A concise construct to make shure we return the right object	
ifValid <- function (x, what, is.not = stop("need a ", what, " object"))
	return(if (inherits(x, what)) valid(x) else is.not)
# res <- ifValid(obj, "class")
## or in a function
# return(ifValid(obj, "class"))

ifElse <- get("ifelse", envir = baseenv())

`%else%` <- function (test, expr) if (test) return(invisible()) else expr
## Useful to write shorter code in something like:
#test %else% break
#test %else% stop(msg)
#test %else% return(res)

## TODO: a tryError(), or some other name making basically
# res <- try(...., silent = TRUE)
# if (inherits(res, "try-error")) stop(msg)

enum <- function (x)
	return(seq_along(x))

## Defines only increasing integer sequences
`%:%` <- function (lower, upper)
	if (lower > upper) integer(0) else
		seq.int(from = as.integer(lower), to = as.integer(upper), by = 1L)
## Useful in:
# for (ii in 1%:%l(v)) print(v)
## Because if (!l(v)) => prints nothing! 1:l(v) would give an error in this case
# for (ii in enum(v) print(v))
## is fine too!

## A better require()
package <- function (package, lib.loc = NULL, silent = TRUE, quietly = silent,
warn.conflicts = silent,
error = stop("there is no package called '", package, "'"))
{
	res <- suppressWarnings(require(package, lib.loc = lib.loc,
		quietly = quietly, warn.conflicts = warn.conflicts,
		character.only = TRUE))
	if (!res) return(error) else return(invisible(res))
}

## Environments management
## Usually, to create an object, we use its name, but
## environment() means something else here!
## So, OK, we'll stick with
newEnv <- get("new.env", envir = baseenv())
## for the moment...

## Now, we want to be able to use names() on it too!
## Note that for environments, we got items by alphabetic order
## => not exactly the same as for vector, list, or so!
names <- function (x)
	if (inherits(x, "environment")) ls(x, all.names = TRUE) else base::names(x)
## Do we implement `names<-` for environments???

## Simpler names for often used functions
n <- get("length", envir = baseenv())
nc <- get("NCOL", envir = baseenv())
nr <- get("NROW", envir = baseenv())

## Constants (must start with an uppercase letter)
## => redefine Pi instead of pi
Pi <- get("pi", envir = baseenv())
## Useful for apply() familly:
Rows <- 1
Cols <- 2
## Instead of apply(x, 2, sum), it gives apply(x, Cols, sum)

## Problem of functional language like R: too much copy!
## For instance, change a simple attribute using attr(x) <- value
## leads to a copy of the object.... If the object is large, time
## needed is significant (+ memory wasted!)
#n <- 1e7
#x <- double(n)
## Trace when x is copied
#tracemem(x)
#system.time(attr(x, "a") <- 1)
## There is a copy of the object => sooo, slow!

## Solution: from data.table...
## 1) setattr() does the same without copying the object
##    but the syntax is not very nice!
## 2) for data.table[, ....] authors define the `:=` function to
##    "assign by reference", i.e., changing a part of a table without
##    copying the object
##
## One could generalize this... plus take advantage of `@` and `@<-` that is
## not used for S3 objects and of the same precedence as `$` to simplify
## manipulation of attributes!

## For non S4 objects, reuse @ for attributes!
## After all, they are, indeed, attributes!
## Note that we force exact match, less error-prone that the opposite,
## and same behaviour as @ used for S4 objects!
## TODO: also use it for S4 object, in the case a slot is not defined
## TODO: add check argument for `attr<-` too
## TODO: attrNames() like slotNames()
## TODO: slots() as a synonym of getSlots() in parallel with attributes()
## NO: getSlots() does not recover the content, but only the class for each
## object in slots =< should reallybe called slotClasses()
## and we need an attrClasses() too!
`@` <- function (object, name)
{
	arg <- substitute(name)
	if (is.name(arg)) name <- as.character(arg)
	if (isS4(object)) slot(object, name) else attr(object, name, exact = TRUE)
}

## Reuse `@<-` to set attribute from a non S4 object
## TODO: also use it for S4 object, in the case a slot is not defined
`@<-` <- function (x, which, value)
{
	arg <- substitute(which)
    if (is.name(arg)) which <- as.character(arg)
	if (isS4(x)) {
		`slot<-`(x, which, check = TRUE, value)
	} else {
		`attr<-`(x, which, value)
	}
}

## Define the "replace by reference" function for attributes, here using
## setattr() from data.table package
## TODO: we need also something like that for S4 slots!
## Since they really are attributes with checking, check first, and then,
## use setattr(), and it is done!
`@:=` <- function (x, which, value)
{
	arg <- substitute(which)
    if (is.name(arg)) which <- as.character(arg)
	if (isS4(x)) {
		## TODO: we need an assign by reference function for S4 slots here
		`slot<-`(x, which, TRUE, value)
	} else {
		setattr(x, which, value)
	}
}

## TODO: `[:=`, `$:=` and `[[:=`
## This does not work...
#`[:=` <- `[<-`

## The`:=` function emulates fun(x) <- value, but with a different mechanism
## that does not imply a copy of x. This is called "replacement by reference"
## in comparison to the usual "replacement by value". It calls `fun:=`
## like fun(x) <- value calls `fun<-`
## TODO: a validation mechanism for the value passed to the function?
## TODO: use alist() instead of list()!!!
`:=` <- function (x, value) {
	call <- match.call()
	X <- substitute(x)
	## pairlist() because NULL would be lost using list()
	value <- pairlist(value = value)
	## In case single name, do the same as x[] <- value, i.e., keeping size
	## and attributes of x ("replacement inside x")
	if (length(X) == 1) {
#		tryCatch(do.call("[<-", c(list(x = X), value), envir = parent.frame(1)),
#			error = function (e) {
#				## Construct a call that is closer to the actual syntax!
#				e$call <- paste(deparse(call[[2]]), ":=", deparse(call[[3]]))
#				stop(e)
#			})
		stop(":= cannot be used directly on an object")
	}
	## If a more complex is provided, try to run `fun:=` instead
	X <- as.pairlist(substitute(X))
	## To emulate `fun<-`, but using `fun:=`
	fun <- paste(deparse(X[[1]]), ":=", sep = "")
	X[[1]] <- NULL
	## Use tryCatch() to ensure a better error message is issued
	tryCatch(do.call(fun, c(X, value), envir = parent.frame(1)),
		error = function (e) {
			## Construct a call that is closer to the actual syntax!
			e$call <- paste(deparse(call[[2]]), ":=", deparse(call[[3]]))
			stop(e)
		})
	## Like for `fun<-`, value is returned invisibly, probably to allow
	## something like x <- y[2] <- value
	return(invisible(value))
}

## I don't like much system.time(), first because it returns 3 numbers where
## we want most of the time only one, and second because it creates a new
## object proc_time, where a difftime object should be perfectly suitable
## => new function elapsed()
timing <- function (expr, gc.first = TRUE)
{
	res <- system.time(expr, gcFirst = gc.first)
	## Results split into result and details
	details <- as.difftime(res[c("user.self", "sys.self")], units = "secs")
	details@names := c("user", "system")
	res <- as.difftime(res["elapsed"], units = "secs")
	res@details := details
	return(res)
}
## Test...
#tst <- timing(Sys.sleep(1.5))
#tst
#tst@details

## Sys.sleep() -> sleep()... no, because sleep is a dataset!!!
#wait <- Sys.sleep
#traceMemory <- tracemem
## From stats: xxx.test() => give a 'htest' object => htestXxxx()
#htestT <- t.test
#htestAnsari <- ansari.test
#htestBatlett <- bartlett.test
#htestChisq <- chisq.test
#htestFisher <- fisher.test
#htestFligner <- fligner.test
#htestFriedman <- friedman.test
#htestKS <- ks.test
#htestMantelHaenszel <- mantelhaen.test
#htestMauchly <- mauchly.test
#htestMcNemar <- mcnemar.test
#htestMood <- mood.test
#htestAnovaPower <- power.anova.test
#htestPropPower <- power.prop.test
#htestTPower <- power.t.test
#htestPhillipsPerron <- PP.test
#htestProp <- prop.test
#htestPropTrend <- prop.trend.test
#htestShapiroWilk <- shapiro.test


#contrHelmert <- contr.helmert
#contrPoly <- contr.poly
#contrSum <- contr.sum
#contrTreatment <- contr.treatment
#contrTreatmentL <- contr.SAS

#equal <- all.equal
#equalA <- attr.all.equal

#baseEnv <- baseenv
#emptyEnv <- emptyenv
#globalEnv <- globalEnv
#parentEnv <- parent.env
#`parentEnv<-` <- `parent.env<-`
##TODO: use tempEnv instead of TempEnv?

#evalParent <- eval.parent

#expandGrid <- expand.grid

#gcTiming <- gc.time + return a difftime object
#gcInfo <- gcinfo
#gcTorture <- gctorture
#??? gcTorture2 <- gctorture2

#inverseRle <- inverse.rle or rleInverse?

#isAtomic <- is.atomic
#isCall <- is.call??
#isElement <- is.element
#?isExpression??
#isFinite <- is.finite
#isLanguage <- is.language
#isLoaded <- is.loaded
#isNA <- is.na
#isNaN <- is.nan
#isNULL <- is.null
#isR <- is.R
#isRecursive <- is.recursive
#isSymbol <- is.symbol
#isUnsorted <- is.unsorted
#isVector <- is.vector
#isTTY <- isatty
#isDebugged <- isdebugged

#l10n.info?
#list2env should be as.environment() applied to list, really
#margin.table, prop.table
#mat.or.vec
#maxCol <- max.col... or colMax, cf. colSum
#average() as a simpler version than mean() for fast run
#onExit <- on.exit

#qrX <- qr.X
#qrQ <- qr.Q
#qrR <- qr.R
# + other qr. functions

#R.home, R.Version, R.version.string
#Recall??
#doCall <- do.call
#rowsum vs rowSums
#enum <- seq_along
#seql <- seq_len
#setdiff & other setxxx functions

#dateCurrent <- Sys.Date
#Sys.getenv, Sys.getlocale, Sys.getpid, Sys.info, Sys.localeconv, sys.on.exit
#sys.parent, Sys.setenv, Sys.setlocale, sys.source, sys.status
#timeCurrent <- Sys.time
#Sys.timezone, Sys.unsetenv

#lowerTri <- lower.tri
#upperTri <- upper.tri

#utf8ToInt



