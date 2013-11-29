## Essentially a series of base R function that manipulate character strings
## and that are renamed/rationalized for facility

## TODO: deal with zero length strings and NAs appropriately in all functions
## TODO: make.names, make.unique, regmatches, grepRaw and charToRaw

## Count the number of characters
## No: make an exception: after n (or nz) do not use uppercase!
#nChar <- nchar
#nzChar <- nzchar

## Format character strings
charEscape <- .Recode(base::encodeString)
charWrap <- base::strwrap
# Add charPad => pad a string left/right or both or charPad/charPadL/charPadR?
#+sprintf/gettextf?

## String find/replace using fixed pattern or regular expressions
rx <- function (pattern, use.bytes = FALSE, globbing,
trim.head = FALSE, trim.tail = TRUE)
{
	## Construct an rx object containing a regular expression pattern
	if (!missing(globbing)) pattern <- utils::glob2rx(globbing,
		trim.head = trim.head, trim.tail = trim.tail)
	pattern <- as.character(pattern)
	class(pattern) <- c("rx", "character")
	if (isTRUE(as.logical(use.bytes))) attr(pattern, "useBytes") <- TRUE
	pattern
}

print.rx <- function (x, ...)
{
	msg <- "Regular expression pattern"
	if (useBytes(x)) {
		msg <- paste(msg, "(byte-by-byte):\n")
	} else msg <- paste(msg, ":\n", sep = "")
	cat(msg)
	print(as.character(x))
}

is.rx <- function (x) inherits(x, "rx")

perl <- function (pattern, use.bytes = FALSE)
{
	pattern <- as.character(pattern)
	class(pattern) <- c("perl", "rx", "character")
	if (isTRUE(as.logical(use.bytes))) attr(pattern, "useBytes") <- TRUE
	pattern
}

print.perl <- function (x, ...)
{
	msg <- "Perl-compatible regular expression pattern"
	if (useBytes(x)) {
		msg <- paste(msg, "(byte-by-byte):\n")
	} else msg <- paste(msg, ":\n", sep = "")
	cat(msg)
	print(as.character(x))
}

is.perl <- function (x) inherits(x, "perl")

useBytes <- function (x) isTRUE(attr(x, "useBytes"))

`useBytes<-` <- function (x, value)
{
	if (!is.character(x)) x <- as.character(x)
	attr(x, "useBytes") <- isTRUE(as.logical(value))
	x
}

charSearch <- function (x, pattern, ignore.case = FALSE,
type = c("logical", "position", "value"), max.distance = 0, costs = NULL)
{
	type <- pmatch(type)
	if (!is.rx(pattern)) { # Search fixed string
		res <- switch(type,
			logical = grepl(pattern, x, ignore.case = ignore.case,
				fixed = TRUE, useBytes = useBytes(pattern)),
			position = grep(pattern, x, ignore.case = ignore.case, value = FALSE,
				fixed = TRUE, useBytes = useBytes(pattern)),
			value = grep(pattern, x, ignore.case = ignore.case, value = TRUE,
				fixed = TRUE, useBytes = useBytes(pattern)),
			stop("Unknown type"))
	} else { # Search regular expression
		## If max.distance > 0, use approximate search
		if (max.distance > 0) { # Use agrep()
			## No perl expression currently accepted!
			if (is.perl(pattern))
				stop("Perl regular expression not allowed with max.distance > 0")
			res <- switch(type,
				logical = 1:length(x) %in% agrep(pattern, x,
					ignore.case = ignore.case, value = FALSE,
					max.distance = max.distance, costs = costs,
					useBytes = useBytes(pattern)),
				position = agrep(pattern, x, ignore.case = ignore.case,
					value = FALSE, max.distance = max.distance, costs = costs,
					useBytes = useBytes(pattern)),
				value = agrep(pattern, x, ignore.case = ignore.case,
					value = TRUE, max.distance = max.distance, costs = costs,
					useBytes = useBytes(pattern)),
				stop("Unknown type"))
		} else { # Use regular search (grep())
			res <- switch(type,
				logical = grepl(pattern, x, ignore.case = ignore.case,
					fixed = FALSE, perl = is.perl(pattern),
					useBytes = useBytes(pattern)),
				position = grep(pattern, x, ignore.case = ignore.case,
					value = FALSE, fixed = FALSE, perl = is.perl(pattern),
					useBytes = useBytes(pattern)),
				value = grep(pattern, x, ignore.case = ignore.case,
					value = TRUE, fixed = FALSE, perl = is.perl(pattern),
					useBytes = useBytes(pattern)),
				stop("Unknown type"))
		}
	}
	res
}

## Inconsistencies: regexpr(pattern, text, ...) and strsplit(x, xplit, ...)
## Solved with the present versions!
charFind <- function (x, pattern, ignore.case = FALSE)
	regexpr(pattern, text = x, ignore.case = ignore.case, fixed = !is.rx(pattern),
		perl = is.perl(pattern), useBytes = useBytes(pattern))
	
charFindAll <- function (x, pattern, ignore.case = FALSE)
	gregexpr(pattern, text = x, ignore.case = ignore.case, fixed = !is.rx(pattern),
		perl = is.perl(pattern), useBytes = useBytes(pattern))

charSub <- function (x, pattern, replacement, ignore.case = FALSE)
	sub(pattern, replacement, x, ignore.case = ignore.case, fixed = !is.rx(pattern),
		perl = is.perl(pattern), useBytes = useBytes(pattern))
	
charSubAll <- function (x, pattern, replacement, ignore.case = FALSE)
	gsub(pattern, replacement, x, ignore.case = ignore.case, fixed = !is.rx(pattern),
		perl = is.perl(pattern), useBytes = useBytes(x))

## Substrings
charSplit <- function (x, pattern)
	strsplit(x, split = pattern, fixed = !is.rx(pattern),
		perl = is.perl(pattern), useBytes = useBytes(pattern))

charSubstr <- .Recode(base::substr)
`charSubstr<-` <- .Recode(base::`substr<-`)
charTrunc <- .Recode(base::strtrim) ## This indeed truncs strings!!!

## paste() is rather long name, in comparison with, e.g., c().
## Also the default argument of sep = " " is irritating and is not consistent
## with stop() or warning() for instance, that use sep = "".
## Thus, we define:
if (exists("paste0", envir = baseenv())) { # Starting from R 2.15.0
	p <- .Recode(base::paste0)
} else {
	p <- function (..., collapse = NULL) 
		paste(..., sep = "", collapse = collapse)
}
	
p_ <- .Recode(base::paste)

## The same is true for cat() with sep = " "... and the default behaviour of
## not ending with line feed is more confusing that useful => change this
## behaviour by adding a end = "\n" argument.
## TODO: by default, interpret unicode and formatting like ucat() or ecat()!
ct <- function (..., file = "", end = "\n", fill = FALSE,
labels = NULL)
	cat(..., end, file = file, sep = "", fill = fill, labels = labels,
		append = FALSE)

cta <- function (..., file = "", end = "\n", fill = FALSE,
labels = NULL)
	cat(..., end, file = file, sep = "", fill = fill, labels = labels,
		append = TRUE)

ct_ <- function (..., file = "", sep = " ", end = "\n", fill = FALSE,
labels = NULL)
	cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = FALSE)

cta_ <- function (..., file = "", sep = " ", end = "\n", fill = FALSE,
labels = NULL)
	cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = TRUE)

	
charTrim <- function (x, all.spaces = FALSE) # Trim both sides
{
	pat <- (if (isTRUE(all.spaces)) "[[:space:]]+" else "[[:blank:]]+")
	## Trim left first
	x <- charSub(p("^", pat), "", x)
	## ... then trim right
	charSub(p(pat, "$"), "", x)
}

charTrimL <- function (x, all.spaces = FALSE) # Trim left-side only
	charSub(if (isTRUE(all.spaces)) "^[[:space:]]+" else "^[[:blank:]]+", "", x)

charTrimR <- function (x, all.spaces = FALSE) # Trim right-side only
	charSub(if (isTRUE(all.spaces)) "[[:space:]]+$" else "[[:blank:]]+$", "", x)


## Change case and translate
charTrans <- function (x, old, new) chartr(old = old, new = new, x = x)
charFold <- base::casefold
charLower <- .Recode(base::tolower)
charUpper <- .Recode(base::toupper)

## Character encoding
encodingToNative <- base::enc2native
encodingToUTF8 <- base::enc2utf8
encoding <- .Recode(base::Encoding)
## R CMD check got fooled because it does not find setEncoding... We give it too
`encoding<-` <- setEncoding <- .Recode(base::`Encoding<-`)

## Measure size of a string (package graphics)
charHeight <- .Recode(graphics::strheight)
charWidth <- .Recode(graphics::strwidth)

## Match, expand or abbreviate character strings to a list of items
charAbbrev <- function (x, min.length = 4, dot = FALSE, strict = FALSE,
method = c("left.kept", "both.sides"))
	abbreviate(names.arg = x, minlength = min.length, dot = dot, strict = strict,
		method = method)

charExpand <- function (x, target, nomatch = NA_character_)
	char.expand(input = x, target = target, nomatch = nomatch)

charMatch <- .Recode(base::charmatch)
## There is a faster version in data.table! named chmatch, but is does not
## exactly the same since it expects characters and do no partial matching!

charPMatch <- .Recode(base::pmatch)

## Conversion to character string... + creation and test, shorter versions
char <- .Recode(base::character)
as.char <- base::as.character
is.char <- base::is.character

# To avoid using strtoi(), we prefer as.integerBase (because as.integer cannot
# be converted into a generic function, because it is a primitive!)
as.intBase <- as.integerBase <- .Recode(base::strtoi)

## Define a function that takes: singular/plural msg and a vector of strings
## and construct a single string with:
## singular msg: single item
## or
## plural msg: item1, item2, ..., itemN
#+paste = cChar? + my special character string manipulation functions?
