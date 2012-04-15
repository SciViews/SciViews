## Essentially a series of base R function that manipulate character strings
## and that are renamed/rationalized for facility

## TODO: deal with zero length strings and NAs appropriately in all functions
## TODO: make.names, make.unique, abbreviate

## Count the number of characters
## No: make an exception: after n (or nz) do not use uppercase!
#nChar <- nchar
#nzChar <- nzchar

## Format character strings
cEscape <- get("encodeString", envir = baseenv())
cWrap <- get("strwrap", envir = baseenv())
# Add cPad => pad a string left/right or both or Pad/PadL/PadR?
#+sprintf/gettextf?

## String find/replace using fixed pattern (char*) or regular expressions (rx*)
## TODO: a rx object which prints an example of its work! => fine-tune it
## to make it easy to experiment with the rx object
rx <- glob2rx # This is in utils package

cSearch <- function (x, pattern, ignore.case = FALSE,
type = c("logical", "position", "value"), ...) # ... for useBytes
{
	type <- pmatch(type)
	res <- switch(type,
		logical = grepl(pattern, x, ignore.case = ignore.case,
			fixed = TRUE, ...),
		position = grep(pattern, x, ignore.case = ignore.case, value = FALSE,
			fixed = TRUE, ...),
		value = grep(pattern, x, ignore.case = ignore.case, value = TRUE,
			fixed = TRUE, ...),
		stop("Unknown type"))
	return(res)
}

rxSearch <- function (x, pattern, ignore.case = FALSE, max.distance = 0,
type = c("logical", "position", "value"), ...) # ... for Perl & useBytes
{
	type <- pmatch(type)
	## If max.distance > 0, use approximate search
	if (max.distance > 0) { # Use agrep()
		res <- switch(type,
			logical = 1:length(x) %in% agrep(pattern, x,
				ignore.case = ignore.case, value = FALSE,
				max.distance = max.distance, ...),
			position = agrep(pattern, x, ignore.case = ignore.case,
				value = FALSE, max.distance = max.distance, ...),
			value = agrep(pattern, x, ignore.case = ignore.case,
				value = TRUE, max.distance = max.distance, ...),
			stop("Unknown type"))
	} else { # Use regular search (grep())
		res <- switch(type,
			logical = grepl(pattern, x, ignore.case = ignore.case,
				fixed = FALSE, ...),
			position = grep(pattern, x, ignore.case = ignore.case,
				value = FALSE, fixed = FALSE, ...),
			value = grep(pattern, x, ignore.case = ignore.case,
				value = TRUE, fixed = FALSE, ...),
			stop("Unknown type"))
	}
	return(res)
}

## Inconsistencies: regexpr(pattern, text, ...) and strsplit(x, xplit, ...)
cFind <- function (x, pattern, ignore.case = FALSE, ...) # ... for useBytes
	return(regexpr(pattern, text = x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxFind <- function (x, pattern, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(regexpr(pattern, text = x, ignore.case = ignore.case, fixed = FALSE,
		...))
	
cFindAll <- function (x, pattern, ignore.case = FALSE, ...) # ... for useBytes
	return(gregexpr(pattern, text = x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxFindAll <- function (x, pattern, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(gregexpr(pattern, text = x, ignore.case = ignore.case, fixed = FALSE,
		...))

cSub <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for useBytes
	return(sub(pattern, replacement, x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxSub <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for Perl & useBytes
	return(sub(pattern, replacement, x, ignore.case = ignore.case, fixed = FALSE,
		...))
	
cSubAll <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for useBytes
	return(gsub(pattern, replacement, x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxSubAll <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for Perl & useBytes
	return(gsub(pattern, replacement, x, ignore.case = ignore.case, fixed = FALSE,
		...))


## Substrings
cSplit <- function (x, pattern, ...) # ... for useBytes
	return(strsplit(x, split = pattern, fixed = TRUE, ...))
	
rxSplit <- function (x, pattern, ...) # for perl & useBytes
	return(strsplit(x, split = pattern, fixed = FALSE, ...))

cSubstr <- get("substr", envir = baseenv())
`cSubstr<-` <- get("substr<-", envir = baseenv())
cTrunc <- get("strtrim", envir = baseenv()) ## This indeed truncs strings!!!

## paste() is rather long name, in comparison with, e.g., c().
## Also the default argument of sep = " " is irritating and is not consistent
## with stop() or warning() for instance, that use sep = "".
## Thus, we define:
if (exists("paste0", envir = baseenv())) { # Starting from R 2.15.0
	p <- get("paste0", envir = baseenv())
} else {
	p <- function (..., sep = "", collapse = NULL) 
		paste(..., sep = sep, collapse = collapse)
}
	
p_ <- get("paste", envir = baseenv())

## The same is true for cat() with sep = " "... and the default behaviour of
## not ending with line feed is more confusing that useful => change this
## behaviour by adding a end = "\n" argument.
## TODO: by default, interpret unicode and formatting like ucat() or ecat()!
ct <- function (..., file = "", sep = "", end = "\n", fill = FALSE,
labels = NULL, append = FALSE)
	return(cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = append))

cta <- function (..., file = "", sep = "", end = "\n", fill = FALSE,
labels = NULL)
	return(cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = TRUE))

ct_ <- function (..., file = "", sep = " ", end = "\n", fill = FALSE,
labels = NULL, append = FALSE)
	return(cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = append))

cta_ <- function (..., file = " ", sep = "", end = "\n", fill = FALSE,
labels = NULL)
	return(cat(..., end, file = file, sep = sep, fill = fill, labels = labels,
		append = TRUE))

	
cTrim <- function (x, all.spaces = FALSE) # Trim both sides
{
	pat <- (if (isTRUE(all.spaces)) "[[:space:]]+" else "[[:blank:]]+")
	## Trim left first
	x <- cSub(p("^", pat), "", x)
	## ... then trim right
	return(cSub(p(pat, "$"), "", x))
}

cTrimL <- function (x, all.spaces = FALSE) # Trim left-side only
{
	pat <- (if (isTRUE(all.spaces)) "^[[:space:]]+" else "^[[:blank:]]+")
	return(cSub(pat, "", x))
}

cTrimR <- function (x, all.spaces = FALSE) # Trim right-side only
{
	pat <- (if (isTRUE(all.spaces)) "[[:space:]]+$" else "[[:blank:]]+$")
	return(cSub(pat, "", x))
}


## Change case and translate
cTrans <- get("chartr", envir = baseenv())
cFold <- get("casefold", envir = baseenv())
cLower <- get("tolower", envir = baseenv())
cUpper <- get("toupper", envir = baseenv())

## Character encoding
encodingToNative <- get("enc2native", envir = baseenv())
encodingToUTF8 <- get("enc2utf8", envir = baseenv())
encoding <- get("Encoding", envir = baseenv())
`encoding<-` <- get("Encoding<-", envir = baseenv())

## Measure size of a string (package graphics)
cHeight <- strheight # From package graphics
cWidth <- strwidth # From package graphics

## Match and expand character strings to a list of items
cExpand <- get("char.expand", envir = baseenv())
cMatch <- get("charmatch", envir = baseenv())
# What to do with pmatch()???

## Conversion to character string... no change required
#as.character

# To avoid using strtoi(), we prefer as.integerBase (because as.integer cannot
# be converted into a generic function, because it is a primitive!)
#charToInt <- strtoi # Allows to choose the base used for char representation
as.integerBase <- get("strtoi", envir = baseenv())

## Define a function that takes: singular/plural msg and a vector of strings
## and construct a single string with:
## singular msg: single item
## or
## plural msg: item1, item2, ..., itemN
#+paste = cChar? + my special character string manipulation functions?

#sAbbreviate <- abbreviate

