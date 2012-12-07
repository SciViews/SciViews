## A series of functions defined or redefined for a simpler or better use of R
## Note to get a function, but change its default parameters, use:
## fun2 <- fun
## formals(fun2)$arg <- newDefaultValue
##
## Note that we should do something about T and F!!!

## Warn when using = instead of <- for assignation...
## if option warnAssignWithEqualSign is TRUE
`=` <- function(x, value)
{
	if (isTRUE(getOption("warnAssignWithEqualSign")))
		warning("Use <- instead of = for assignation, or use == for equalty test")
	assign(deparse(substitute(x)), value, envir = parent.frame())
}

# is.wholenumber(), see ?as.integer => define isWholeInt?

## A convenient starting object for holding items: . == .GlobalEnv
## TODO: take care there is no clash with proto objects!
#. <- base::.GlobalEnv

## Testing is.null(obj) is not enough to decide if an object is empty, because
## there may be like numeric(0), character(0), etc. The right way to do so is
## to use if (!length(obj)), but it would be more intuitive to define:
## TODO: isEmpty is a generic function (or even S4?) in filehash that does
##       something different => change the name!
isEmpty <- function (x) !length(x)

ifElse <- get("ifelse", envir = baseenv())

`%else%` <- function (test, expr) if (test) return(invisible()) else expr
## Useful to write shorter code in something like:
#test %else% break
#test %else% stop(msg)
#test %else% return(res)

## TODO: a tryError(), or some other name making basically
# res <- try(...., silent = TRUE)
# if (inherits(res, "try-error")) stop(msg)

enum <- function (x) seq_along(x)

## Defines only increasing integer sequences
`%:%` <- function (lower, upper)
	if (lower > upper) integer(0) else
		seq.int(from = as.integer(lower), to = as.integer(upper), by = 1L)
## Useful in:
# for (ii in 1%:%l(v)) print(v)
## Because if (!l(v)) => prints nothing! 1:l(v) would give an error in this case
# for (ii in enum(v)) print(v)
## is fine too!

## A better require()
package <- function (package, lib.loc = NULL, silent = TRUE, quietly = silent,
warn.conflicts = silent,
error = stop("there is no package called '", package, "'"))
{
	res <- suppressWarnings(require(package, lib.loc = lib.loc,
		quietly = quietly, warn.conflicts = warn.conflicts,
		character.only = TRUE))
	if (!res) error else invisible(res)
}

## Now, we want to be able to use names() on environments too!
## Note that for environments, we got items by alphabetic order
## => not exactly the same as for vector, list, or so!
names <- function (x)
	if (inherits(x, "environment")) ls(x, all.names = TRUE) else base::names(x)
## Do we implement `names<-` for environments??? This is a nonsense, may be?

## Simpler names for often used functions
num <- base::as.numeric
int <- base::as.integer
char <- base::as.character
logic <- base::as.logical
## To avoid problems with factors, tell to always use s(f1), or n(f1)/i(f1)

## Since n is already used for a synonym of as.numeric(), I use l() here
l <- base::length
nc <- base::NCOL
nr <- base::NROW

## Constants (must start with an uppercase letter)
## => redefine Pi instead of pi
Pi <- base::pi
## Useful for apply() familly:
Rows <- 1
Cols <- 2
## Instead of apply(x, 2, sum), it gives apply(x, Cols, sum)

## I don't like isTRUE, because if there is an attribute attached to TRUE,
## it returns FALSE! => define asTRUE which is more permissive!
asTRUE <- function (x) identical(TRUE, as.logical(x))
isFALSE <- function (x) identical(FALSE, x)
asFALSE <- function (x) identical(FALSE, as.logical(x))

## How to simplify the use of if() by limiting possible special cases?
## use of any() and all() is there to cope with this, but still:
## 1) any(NA) => NA, unless any(NA, na.rm = TRUE) => FALSE
## 2) any(NULL) & any(logical(0)) => FALSE => OK
## We solve this by defining any.() and all.()
any. <- function (..., na.rm = TRUE) any(..., na.rm = na.rm)
all. <- function (..., na.rm = TRUE) all(..., na.rm = na.rm)
one <- function (x, na.rm = FALSE) UseMethod("one")
## Same as asTRUE(), but slower, because it is a method
one.default <- function (x, na.rm = FALSE)
{
	if (isTRUE(na.rm)) x <- na.omit(x)
	return(identical(TRUE, as.logical(x)))
}
one. <- function (x, na.rm = TRUE) one(x, na.rm = na.rm)
stopIfNot <- base::stopifnot

## TODO: other xxx. functions for those using na.rm = FALSE
## like mean, median, sd, var, quantile, fivenum, ...

`%is%` <- function (x, class) is(x, as.character(substitute(class)))
`%as%` <- function (x, class) as(x, as.character(substitute(class)))

#s1 <- 12.3
#s1 %is% numeric
#s1 %is% integer
#s1 %as% integer %is% integer

## Ternary condition statement, like in JavaScript cond ? yes : no
## Not possible to do in R... but the closest is:
#`%?%` <- function (cond, yes.no) { if (cond) yes.no[1] else yes.no[2] }
## ... and its vectorized conterpart:
#`%??%` <- function (cond, yes.no) ifelse(cond, yes = yes.no[1], no = yes.no[2])
#TRUE %?% c(1, 2)
#FALSE %?% c(yes = 1, no = 2)
#x <- 1:3
#res <- any(x > 2) %?% c("yes", "no"); res
#res <- (x > 2) %??% c("yes", "no"); res # Take care of parentheses!
#rm(x, res)

## It is common to test if something is zero, or one... Here, the non vectorized
## version asks for all items being zero or one, excluding missing data!
## TODO: good idea (perhaps)... but this does not work well!
#`%?0%` <- function (x, yes.no) { if (all.(x == 0)) yes.no[1] else yes.no[2] }
#`%?1%` <- function (x, yes.no) { if (all.(x == 1)) yes.no[1] else yes.no[2] }
#`%??0%` <- function (x, yes.no) ifelse(x == 0, yes = yes.no[1], no = yes.no[2])
#`%??1%` <- function (x, yes.no) ifelse(x == 1, yes = yes.no[1], no = yes.no[2])
#x <- 1; x %?0% c(yes = stop("x must be non null"), no = x^2)
#x <- 0; x %?0% c(yes = stop("x must be non null"), no = x^2)
## This helps to construct sentences with single or plural
#x <- 1; rep(x, 3) %??1% c(single = c("There is ", 1, " item in x"),
#	plural = c("There are ", length(x), " items in x"))
#x <- 3; rep(x, 3) %??1% c(single = c("There is ", 1, " item in x"),
#	plural = c("There are ", length(x), " items in x"))


## Should we keep these without renaming???
#na.action()
#na.omit()
#na.fail()
#na.exclude()
#na.pass()
## And what to do with naresid() and napredict()?

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
	## If a more complex call is provided, try to run `fun:=` instead
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

#all.names
#all.vars

#Data and POSIXct

#contrHelmert <- contr.helmert
#contrPoly <- contr.poly
#contrSum <- contr.sum
#contrTreatment <- contr.treatment
#contrTreatmentL <- contr.SAS

#equal <- all.equal
#equalA <- attr.all.equal

#expandGrid <- expand.grid

#gcTiming <- gc.time + return a difftime object
#gcInfo <- gcinfo
#gcTorture <- gctorture
#??? gcTorture2 <- gctorture2

#rleInverse <- inverse.rle

#No -> isAtomic <- is.atomic
#No -> isCall <- is.call??
#isElement <- is.element
#?isExpression??
#isFinite <- is.finite
#No -> isLanguage <- is.language
#isLoaded <- is.loaded
#isNA <- is.na
#isNaN <- is.nan
#isNULL <- is.null
#isR <- is.R
#No -> isRecursive <- is.recursive
#No -> isSymbol <- is.symbol
#isUnsorted <- is.unsorted
#No -> isVector <- is.vector
#isTTY <- isatty

#l10n.info?
#list2env should be as.environment() applied to list, really
#margin.table, prop.table
#mat.or.vec
#maxCol <- max.col... or colMax, cf. colSum
#average() as a simpler version than mean() for fast run

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
#Sys.getenv, Sys.getlocale, Sys.getpid, Sys.info, Sys.localeconv
#Sys.setenv, Sys.setlocale
#timeCurrent <- Sys.time
#Sys.timezone, Sys.unsetenv

#lowerTri <- lower.tri
#upperTri <- upper.tri

#utf8ToInt
#cStackInfo <- base::Cstack_info
#.Internal() triggers notes => what to do?
#.Primitive()

## Read this carefully before rethinking these function, trying to simplify a bit things:
## http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
## Environments management
## Use of frame as a synonym of environment brings an additional difficulty on
## an already difficult subject! => use env(ironment) everywhere?!
## TODO: all these sys.xxx must remain like this!
sysFunction <- .Recode(base::sys.function)
sysCall <- .Recode(base::sys.call)
sysCalls <- .Recode(base::sys.calls)
matchCall <- .Recode(base::match.call)
sysParent <- .Recode(base::sys.parent)
sysParents <- .Recode(base::sys.parents)
## TODO: do not use frame => what??? sys.prevEnv()??
parentFrame <- .Recode(base::parent.frame)
sysFrame <- .Recode(base::sys.frame)
sysFrames <- .Recode(base::sys.frames)
sysnFrame <- .Recode(base::sys.nframe)
sysStatus <- base::sys.status
onExit <- function (expr = NULL, add = FALSE) base::on.exit(expr = expr, add = add)
sysOnExit <- .Recode(base::sys.on.exit)
dumpFrames <- utils::dump.frames
#debugger(dump = last.dump) # utils
#browser()
#browserText()
#browserCondition()
#browserSetDebug()
#debug()
#undebug()
debugOnce <- .Recode(base::debugonce)
isDebugged <- .Recode(base::isdebugged)
baseEnv <- base::baseenv
.BaseEnv <- base::baseenv()
baseNamespaceEnv <- function () return(.BaseNamespaceEnv)
#.BaseNamespaceEnv already defined
## Those four environments are specials and start with an uppercase letter!
emptyEnv <- base::emptyenv
.EmptyEnv <- base::emptyenv()
globalEnv <- base::globalenv # Also .GlobalEnv
# .GlobalEnv already defined
autoloadEnv <- function () return(.AutoloadEnv)
#.AutoloadEnv already defined
#TempEnv() in svMisc
tempEnv <- svMisc::TempEnv
.TempEnv <- svMisc::TempEnv()
## TODO: or sys.topEnv()???
## RCMD check claims he cannot find isNamespaceEnv() in topEnv() => provide it
isNamespaceEnv <- function (envir = parentFrame())
	.Intern(isNamespaceEnv(envir))
topEnv <- .Recode(base::topenv)
# Usually, to create an object, we use its name, but
## environment() means something else here!
## So, OK, we'll stick with:
environmentNew <- .Recode(base::new.env)
## Should not be used!
environmentParent <- .Recode(base::parent.env)
`environmentParent<-` <- .Recode(base::`parent.env<-`)
#environmentName()
#environment()
#`environment<-`()
#is.environment()
environmentProfile <- .Recode(base::env.profile)
## name attribute to an environment,... see ?environment
#source()

sysSource <- base::sys.source
#.First.sys and .Last.sys cannot be changed!
#eval()
evalQuote <- .Recode(base::evalq)
evalParent <- base::eval.parent
evalLocal <- base::local

autoloaded <- function () return(.Autoloaded)
#autoload()
#autoloader()
#delayedAssign()

## This is the options() mechanism:
## I don't like the options("width") returning a list with only $width in it!
## I want a mechanisms much like par("ask") which directly returns the value, thus:
### Covered function: base::options(), base::getOption(), base::.Options
opt <- function (...) {
	arg <- list(...)
	l <- length(arg)
	if (l == 0) {
		return(options()) # List of all options
	} else if (l == 1 && is.null(names(arg))) {
		return(options(...)[[1]]) # The value for this option
	} else return(invisible(options(...))) # Invisible list of previous options
}
## With a single argument, opt() and optDef() give the same thing, but
## optDef() allows to provide a default value for the option, if not found
optDef <- getOption # (x, default = NULL)


## For R help on the web:
## http://rseek.org
## http://www.r-project.org/mail.html for mailing lists
## StackOverflow http://stackoverflow.com/questions/tagged/r
## #rstats Twitter hashtag http://search.twitter.com/search?q=%23rstats
## R-Bloggers http://www.r-bloggers.com
## Video Rchive (of presentations) http://www.vcasmo.com/user/drewconway

## Useful packages from "machine learning for hackers"
## arm, glmnet, ggplot2, igraph, lme4, lubridate, RCurl, plyr, RJSONIO, spatstat, RSXML

## ! is not defined for character strings... Use it here for quick conversion
## of character into an "s" (string) object... Used in doc blocks for an R script
## compatible with Sweave
`!` <- function(x) if (is.character(x))
	structure(x, class = c("s", "character")) else .Primitive("!")(x)

## The print.s method is designed to print nothing in case of a doc block
## TODO: need methods to convert these into Html or Pdf for quick view!
## TODO: a method to check correctness of these blocks for Asciidoc blocks
## (for LaTeX blocks, it would not work)
print.s <- function (x, ...)
{
	## If the string starts with @\n and ends with <<.*>>=,
	## treat it specially (it is a doc chunk!): print just nothing!
	if (grepl("^@[ \t]*\n.*<<[^\n]*>>=[ \t]*$", x)) {
		cat("<...doc chunk...>\n")
	} else print(as.character(x))
	return(invisible(x))
}
