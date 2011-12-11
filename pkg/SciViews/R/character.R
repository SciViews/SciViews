## Essentially a series of base R function that manipulate character strings
## and that are renamed/rationalized for facility
## TODO: deal with zero length strings and NAs appropriately in all functions

## Count the number of characters
## No: make an exception: after n (or nz) do not use uppercase!
#nChar 				<- nchar
#nzChar				<- nzchar

## Format character strings
strEscape			<- encodeString
strWrap				<- strwrap
# Add strPad => pad a string left/right or both or Padb/Padl/Padr?
#+sprintf/gettextf?

## String find/replace using fixed pattern (str*) or regular expressions (reg*)
## TODO: a rx object which prints an example of its work! => fine-tune it
## to make it easy to experiment with the rx object
rx <- glob2rx

strFind <- function (x, pattern, ignore.case = FALSE,
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

rxFind <- function (x, pattern, ignore.case = FALSE, max.distance = 0,
type = c("logical", "position", "value"), ...) # ... for perl & useBytes
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

strSearch <- function (x, pattern, ignore.case = FALSE, ...) # ... for useBytes
	return(regexpr(pattern, text = x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxSearch <- function (x, pattern, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(regexpr(pattern, text = x, ignore.case = ignore.case, fixed = FALSE,
		...))
	
strSearchAll <- function (x, pattern, ignore.case = FALSE, ...) # ... for useBytes
	return(gregexpr(pattern, text = x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxSearchAll <- function (x, pattern, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(gregexpr(pattern, text = x, ignore.case = ignore.case, fixed = FALSE,
		...))

strReplace <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for useBytes
	return(sub(pattern, replacement, x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxReplace <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(sub(pattern, replacement, x, ignore.case = ignore.case, fixed = FALSE,
		...))
	
strReplAll <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for useBytes
	return(gsub(pattern, replacement, x, ignore.case = ignore.case, fixed = TRUE,
		...))
	
rxReplAll <- function (x, pattern, replacement, ignore.case = FALSE, ...) # ... for perl & useBytes
	return(gsub(pattern, replacement, x, ignore.case = ignore.case, fixed = FALSE,
		...))


## Substrings
strSplit <- function (x, pattern, ...) # for useBytes
	return(strsplit(x, split = pattern, fixed = TRUE, ...))
	
rxSplit <- function (x, pattern, ...) # for perl & useBytes
	return(strsplit(x, split = pattern, fixed = FALSE, ...))

strSub				<- substr
`strSub<-`			<- `substr<-`
strTrunc			<- strtrim ## This indeed truncs strings!!!

## paste() is rather long name, in comparison with, e.g., c().
## Also the default argument of sep = " " is irritating and is not consistent
## with stop() or warning() for instance.
## Thus, we define:
p <- function (..., sep = "", collapse = NULL) 
	.Internal(paste(list(...), sep, collapse))
	
p_  <- paste

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

	
strTrimb <- function (x, all.spaces = FALSE) # Trim both sides
{
	pat <- (if (isTRUE(all.spaces)) "[[:space:]]+" else "[[:blank:]]+")
	## Trim left first
	x <- strReplace(p("^", pat), "", x)
	## ... then trim right
	return(strReplace(p(pat, "$"), "", x))
}

strTriml <- function (x, all.spaces = FALSE) # Trim left-side only
{
	pat <- (if (is.TRUE(all.spaces)) "^[[:space:]]+" else "^[[:blank:]]+")
	return(strReplace(pat, "", x))
}

strTrimr <- function (x, all.spaces = FALSE) # Trim right-side only
{
	pat <- (if (is.TRUE(all.space)) "[[:space:]]+$" else "[[:blank:]]+$")
	return(strReplace(pat, "", x))
}


## Change case and translate
strTr()				<- chartr
strCaseFold()		<- casefold
strLower()			<- tolower
strUpper()			<- toupper

## Character encoding
encodingToNative()	<- enc2native
encodingToUTF8()	<- enc2utf8
encoding()			<- Encoding
`encoding<-`		<- `Encoding<-`

## Measure size of a string (package graphics)
strHeight()			<- strheight
strWidth()			<- strwidth

## Match and expand character strings to a list of items
strExpand()			<- char.expand
strMatch()			<- charmatch
# What to do with pmatch()???

## Conversion to character string
#as.character

# To avoid using strtoi(), we prefer as.integerBase (because as.integer cannot
# be converted into a generic function, because it is a primitive!)
#strToInt()			<- strtoi # Allows to choose the base used for char representation
as.integerBase 		<- strtoi

#+paste = cChar? + my special character string manipulation functions?
# is.wholenumber(), see ?as.integer => define isWholeInt?

## This should be nice:
## Define a valid method to be applied to S3 objects to make sure they are
## correct
valid <- function (object, ...)
	UseMethod("valid")
	
valid.default <- function (object, ...)
	return(object)

ifIs <- function (x, what, yes = valid(x),
no = stop("need a ", what, " object"))
	return(if (inherits(x, what)) yes else no)

ifElse			<- ifelse

## This is useful to get something similar to df$var or obj@slot
## TODO: how to solve the case ll%a%metadata$OK for metadata being a list?
`%a%` <- function (x, which)
	return(attr(x, deparse(substitute(which)), exact = FALSE))
	
`%a%<-` <- function (x, which, value)
	return(`attr<-`(x, deparse(substitute(which)), value))

## To be consistent with the other extraction functions:
a <- function (x, which, exact = TRUE)
	return(attr(x, which, exact))

## Environments management
## Usually, to create an object, we use its name, but
## environment() means something else here!
## So, OK, we'll stick with
newEnv <- new.env
## for the moment...
## Now, we want to be able to use names() on it too!
## Note that for environments, we got items by alphabetic order
## => not exactly the same as for vector, list, or so!
names <- function (x)
	if (inherits(x, "environment")) ls(x, all = TRUE) else base::names(x)
## Do we implement `names<-` for environments???

## A more convenient setwd()/getwd() using objects
wdir <- function (dir = NULL)
{
	if (is.null(dir)) {
		dir <- getwd()
		class(dir) <- c("filename", "character")
		## Make sure to use /, even under Windows
		dir <- gsub("\\\\", "/", dir)
		return(dir)
	} else { # Change current working directory
		owdir <- setwd(dir)
		## Make sure to use /, even under Windows
		owdir <- gsub("\\\\", "/", owdir)
		class(owdir) <- c("filename", "character")
		## Save old working directory
		.owdir <<- owdir
		return(owdir)
	}
}

## Get or set session dir
sdir <- function (dir = NULL)
{
	if (is.null(dir)) {
		dir <- getOption("R.initdir")
		if (is.null(dir)) return(NULL)
		class(dir) <- c("filename", "character")
		## Make sure to use /, even under Windows
		dir <- gsub("\\\\", "/", dir)
		return(dir)
	} else { # Change current session directory
		osdir <- getOption("R.initdir")
		## TODO: make sure to do everything required to cleanly close current
		## session!
		dir <- gsub("\\\\", "/", dir)
		options(R.initdir = dir)
		## TODO: make everything we need to open the new session directory
		## Make sure to use /, even under Windows
		osdir <- gsub("\\\\", "/", osdir)
		class(osdir) <- c("filename", "character")
		## Save old session directory
		.osdir <<- osdir
		return(osdir)
	}
}


subclass <- function (x, class, superclasses = NULL)
{
	## TODO: check this is an S3 object that inherits from the gicven class(es)
	if (!is.null(superclasses)) {
		misClass <- inherits(x, as.character(superclasses), which = TRUE) == 0
		if (any(misClass))
			stop("'x' soes not inherits from", paste(superclasses[misClass],
				collapse = ", "))
	}
	## Check if new class in not already defined
	if (class %in% class(x)) return(x)
	## Prepend that class
	class(x) <- c(class, class(x))
	return(x)
}

`subclass<-` <- function (x, value)
{
	if (!value %in% class(x)) class(x) <- c(value, class(x))
	return(x)
}

filename <- function (...)
{
	## Create a vector of filename objects inheriting from character
	return(subclass(as.character(c(...)), "filename"))
}

print.filename <- function (x, ...)
{
	path <- as.character(x)
	path <- gsub("\\\\", "/", path)
	## Make sure paths are ended with / to differentiate them from files 
	isdir <- file.info(path)$isdir
	## Non-existent files are these ones
	nofile <- is.na(isdir)
	path[nofile] <- paste(path[nofile], "*", sep = "")
	## These are directories
	isdir <- (isdir & !grepl("/$", path))
	isdir[is.na(isdir)] <- FALSE
	path[isdir] <- paste(path[isdir], "/", sep = "")
	## Print it
	print(noquote(paste("<", path, ">", sep = "")))
	return(invisible(x))
}
