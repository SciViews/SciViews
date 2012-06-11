\name{character-fun}
\alias{ct}
\alias{cta}
\alias{ct_}
\alias{cta_}
\alias{p}
\alias{p_}
\alias{cTrans}
\alias{cFold}
\alias{cLower}
\alias{cUpper}
\alias{cSplit}
\alias{cTrim}
\alias{cTrimL}
\alias{cTrimR}
\alias{cTrunc}
\alias{cSubstr} 
\alias{cSubstr<-}
\alias{cSearch}
\alias{cFind}
\alias{cFindAll}
\alias{cSub}
\alias{cSubAll}
\alias{cMatch}
\alias{cPMatch}
\alias{cExpand}
\alias{cAbbreviate}
\alias{cEscape}
\alias{cWrap}
\alias{cHeight}
\alias{cWidth}

\title{Character strings manipulation functions}

\description{
  A series of functions to manipulate character objects.
}

\usage{
## Concatenate and print strings to the console or in a file
ct(\dots, file = "", end = "\n", fill = FALSE, labels = NULL)
cta(\dots, file = "", end = "\n", fill = FALSE, labels = NULL)
ct_(\dots, file = "", sep = " ", end = "\n", fill = FALSE, labels = NULL)
cta_(\dots, file = "", sep = " ", end = "\n", fill = FALSE, labels = NULL)

## Paste strings together
p(\dots, collapse = NULL)
p_(\dots, sep = " ", collapse = NULL)

## character translation or folding
cTrans(x, old, new)
cFold(x, upper = FALSE)
cLower(x)
cUpper(x)

## Split, truncate or work with substrings
cTrim(x, all.spaces = FALSE)
cTrimL(x, all.spaces = FALSE)
cTrimR(x, all.spaces = FALSE)
cTrunc(x, width)
cSubstr(x, start, stop) 
cSubstr(x, start, stop) <- value
cSplit(x, pattern)

## Find and replace in character strings
cSub(x, pattern, replacement, ignore.case = FALSE)
cSubAll(x, pattern, replacement, ignore.case = FALSE)
cFind(x, pattern, ignore.case = FALSE)
cFindAll(x, pattern, ignore.case = FALSE)
cSearch(x, pattern, ignore.case = FALSE,
    type = c("logical", "position", "value"), max.distance = 0, costs = NULL)

## Match, expand or abbreviate character strings
cMatch(x, table, nomatch = NA_integer_) 
cPMatch(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
cExpand(x, target, nomatch = NA_character_)
cAbbreviate(x, min.length = 4, dot = FALSE, strict = FALSE,
    method = c("left.kept", "both.sides"))

## Format character strings
cEscape(x, width = 0L, quote = "", na.encode = TRUE,
    justify = c("left", "right", "centre", "none")) 
cWrap(x, width = 0.9 * getOption("width"), indent = 0, exdent = 0, 
    prefix = "", simplify = TRUE, initial = prefix)

## Measure size of a character string or expression in a graph
cHeight(s, units = "user", cex = NULL, font = NULL, vfont = NULL, \dots)
cWidth(s, units = "user", cex = NULL, font = NULL, vfont = NULL, \dots)
}

\arguments{
  \item{\dots}{ a series of character strings or objects that can be coerced to
    character. }
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
  \item{x}{ a character string or an object that can be coerced to character. }
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
  All these functions start with a lowercase c before an uppercase letter to
  indicate that they operate on, character strings and coerce their first
  argument \code{x} to a character object.
  
  \code{cEscape(x)} ...
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
