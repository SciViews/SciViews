\name{char-fun}
\alias{char}
\alias{as.char}
\alias{is.char}
\alias{ct}
\alias{cta}
\alias{ct_}
\alias{cta_}
\alias{p}
\alias{p_}
\alias{charTrans}
\alias{charFold}
\alias{charLower}
\alias{charUpper}
\alias{charSplit}
\alias{charTrim}
\alias{charTrimL}
\alias{charTrimR}
\alias{charTrunc}
\alias{charSubstr} 
\alias{charSubstr<-}
\alias{charSearch}
\alias{charFind}
\alias{charFindAll}
\alias{charSub}
\alias{charSubAll}
\alias{charMatch}
\alias{charPMatch}
\alias{charExpand}
\alias{charAbbrev}
\alias{charEscape}
\alias{charWrap}
\alias{charHeight}
\alias{charWidth}

\title{Character strings manipulation functions}

\description{
  A series of functions to manipulate character objects.
}

\usage{
## Create, test or coerce to character
char(length = 0)
as.char(x, \dots)
is.char(x)

## Concatenate and print strings to the console or in a file
ct(\dots, file = "", end = "\n", fill = FALSE, labels = NULL)
cta(\dots, file = "", end = "\n", fill = FALSE, labels = NULL)
ct_(\dots, file = "", sep = " ", end = "\n", fill = FALSE, labels = NULL)
cta_(\dots, file = "", sep = " ", end = "\n", fill = FALSE, labels = NULL)

## Paste strings together
p(\dots, collapse = NULL)
p_(\dots, sep = " ", collapse = NULL)

## character translation or folding
charTrans(x, old, new)
charFold(x, upper = FALSE)
charLower(x)
charUpper(x)

## Split, truncate or work with substrings
charTrim(x, all.spaces = FALSE)
charTrimL(x, all.spaces = FALSE)
charTrimR(x, all.spaces = FALSE)
charTrunc(x, width)
charSubstr(x, start, stop) 
charSubstr(x, start, stop) <- value
charSplit(x, pattern)

## Find and replace in character strings
charSub(x, pattern, replacement, ignore.case = FALSE)
charSubAll(x, pattern, replacement, ignore.case = FALSE)
charFind(x, pattern, ignore.case = FALSE)
charFindAll(x, pattern, ignore.case = FALSE)
charSearch(x, pattern, ignore.case = FALSE,
    type = c("logical", "position", "value"), max.distance = 0, costs = NULL)

## Match, expand or abbreviate character strings
charMatch(x, table, nomatch = NA_integer_) 
charPMatch(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
charExpand(x, target, nomatch = NA_character_)
charAbbrev(x, min.length = 4, dot = FALSE, strict = FALSE,
    method = c("left.kept", "both.sides"))

## Format character strings
charEscape(x, width = 0L, quote = "", na.encode = TRUE,
    justify = c("left", "right", "centre", "none")) 
charWrap(x, width = 0.9 * getOption("width"), indent = 0, exdent = 0, 
    prefix = "", simplify = TRUE, initial = prefix)

## Measure size of a character string or expression in a plot
charHeight(s, units = "user", cex = NULL, font = NULL, vfont = NULL, \dots)
charWidth(s, units = "user", cex = NULL, font = NULL, vfont = NULL, \dots)
}

\arguments{
  \item{length}{ the length of the character vector to create. }
  \item{x}{ a character string or an object that can be coerced to character or
    tested as such. }
  \item{\dots}{ a series of character strings or objects that can be coerced to
    character, or further arguments passed to or from other methods for
    \code{as.char()}. }
  \item{file}{ a character string naming a file, or a connection. The default
    \code{""} prints to the standard output connection (see \code{\link[base]{cat}}). }
  \item{end}{ the character string to append at the end. By default, a carriage
    return. }
  \item{fill}{ a logical or positive numeric. With \code{FALSE} (default), only
    carriage returns (\code{"\n"}) create new lines. With \code{TRUE}, strings
    are formatted within \code{getOption("width")}. With a positive integer,
    strings are formatted within this number of characters. }
  \item{labels}{ labels of the lines printed. Ignored when \code{fill = FALSE}. }
  \item{sep}{ the character string to use to separate successive strings. }
  \item{collapse}{ an optional character string to separate items in a character
    vector that is collapsed to a single item. }
  \item{old}{ a character string with characters to be translated. First element
    is used with a warning if length is higher than two. }
  \item{new}{ a character string with the translations. First element
    is used with a warning if length is higher than two. }
  \item{upper}{ a logical indicating if conversion is done to upper- or lowercase. }
  \item{all.spaces}{ eliminate all spacing characters, or only blanks? }
  \item{width}{ todo }
  \item{start}{ todo }
  \item{stop}{ todo }
  \item{value}{ todo }
  \item{pattern}{ todo }
  \item{replacement}{ todo }
  \item{ignore.case}{ todo }
  \item{type}{ todo }
  \item{max.distance}{ todo }
  \item{costs}{ todo }
  \item{table}{ todo }
  \item{nomatch}{ todo }
  \item{duplicates.ok}{ todo }
  \item{target}{ todo }
  \item{min.length}{ todo }
  \item{dot}{ todo }
  \item{strict}{ todo }
  \item{method}{ todo }
  \item{quote}{ todo }
  \item{na.encode}{ todo }
  \item{justify}{ todo }
  \item{indent}{ todo }
  \item{exdent}{ todo }
  \item{prefix}{ todo }
  \item{simplify}{ todo }
  \item{initial}{ todo }
  \item{s}{ todo }
  \item{units}{ todo }
  \item{cex}{ todo }
  \item{font}{ todo }
  \item{vfont}{ todo }
}

\details{
  All these functions start with char before an uppercase letter to
  indicate that they operate on character strings or coerce their first
  argument \code{x} to a character object.
  
  \code{charEscape(x)} ...
}

\value{
  Most of these functions return a modified character string object, except ...
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but most functions are wrappers
  around existing functions written by the R Core Team in base, or recommended
  packages.
}

\seealso{ \code{\link[base]{character}} } %, \code{\link{rx}} }

\examples{
## TODO: various examples of character string manipulations 
}

\keyword{character}

\concept{ character strings manipulation }
