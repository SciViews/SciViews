\name{file}
\alias{filePath}
\alias{print.filePath}
\alias{as.filePath}
\alias{is.filePath}
\alias{isDir}
\alias{isFile}
\alias{fileName}
\alias{fileDir}
\alias{fileExpand}
\alias{fileNormalize}
\alias{dirR}
\alias{filePackage}
\alias{dirTemp}
\alias{fileTemp}
\alias{fileFind}
\alias{dirList}
\alias{fileList}
\alias{fileListGlob}
\alias{dirCreate}
\alias{fileAccess}
\alias{fileAppend}
\alias{fileRename}
\alias{fileCopy}
\alias{fileCreate}
\alias{fileExists}
\alias{fileInfo}
\alias{fileChmod}
\alias{fileUMask}
\alias{fileTime}
\alias{fileRemove}
\alias{fileDelete}
\alias{fileLink}
\alias{fileSymLink}
\alias{fileReadLink}
\alias{fileShow}
\alias{wdir}
\alias{sdir}


\title{Files and directories manipulation}

\description{
  Manipulate files and directories and create or use 'filePath' objects.
}

\usage{
## Create, test or print a filePath object
filePath(\dots, fsep = .Platform$file.sep)
is.filePath(x)
as.filePath(x, \dots)
\method{print}{filePath}(x, \dots)

## Test or manipulate path for files or directories
isDir(filePath)
isFile(filePath)
fileName(filePath)
fileDir(filePath)
fileExpand(filePath)
fileNormalize(filePath, mustWork = FALSE)

## Look for R-associated files or dirs
dirR(component = "home")
filePackage(\dots, package = "base", lib.loc = NULL, mustWork = FALSE)

## Temporary directories or files
dirTemp()
fileTemp(pattern = "file", tmpdir = tempdir(), fileext = "")

## List of find files and directories
fileFind(names)
dirList(filePath = ".", full.names = TRUE, recursive = TRUE)
fileList(filePath = ".", pattern = NULL, all.files = FALSE, full.names = FALSE,
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
fileListGlob(filePath, dir.mark = FALSE)

## Create, delete, append, etc. files
dirCreate(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
fileAccess(names, mode = 0)
fileAppend(file1, file2)
fileRename(from, to)
fileCopy(from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE)
fileCreate(\dots, showWarnings = TRUE)
fileExists(\dots)

fileChmod(paths, mode = "0777", use_umask = TRUE)
fileUMask(mode = NA)
fileTime(filePath, time)
fileRemove(\dots)
fileDelete(filePath, recursive = FALSE, force = FALSE)

## File links and symbolic links
fileLink(from, to)
fileSymLink(from, to)
fileReadLink(filePath)

## Show the content of a file
fileShow(\dots, header = rep("", nfiles), title = "R Information",
    delete.file = FALSE, pager = getOption("pager"), encoding = "")

## Working or session directory
wdir(dir = NULL)
sdir(dir = NULL)
}

\arguments{
  \item{\dots}{ character vector containing file items or file paths. }
  \item{fsep}{ character used as file separator. }
  \item{x}{ an object. }
  \item{filePath}{ a filePath object. }
  \item{component}{ As well as \code{"home"} which gives the R home directory,
    other known values are \code{"bin"}, \code{"doc"}, \code{"etc"},
    \code{"modules"} and \code{"share"} giving the paths to the corresponding
    parts of an R installation. }
  \item{package}{ the R package to look for file. }
  \item{lib.loc}{ the library path to look for the R package. }
  \item{mustWork}{ should it always return? }  
  \item{pattern}{ pattern to use for the temporary file. }
  \item{tmpdir}{ temporary directory. }
  \item{fileext}{ extension of the temporary file. }
  \item{names}{ file names to find. }
  \item{full.names}{ return full path names or only file names? }
  \item{recursive}{ list dirs or files recursively? }
  \item{all.files}{ return all files? }
  \item{ignore.case}{ be case insensitive? }
  \item{include.dirs}{ also list directories? }
  \item{dir.mark}{ logical: should matches to directories from patterns that do
    not already end in / have a slash appended? May not be supported on all
    platforms. }
  \item{path}{ path to file. }
  \item{showWarnings}{ warn if dir cannot be created? }
  \item{mode}{ Unix mode of the file or dir. }
  \item{file1}{ first file. }
  \item{file2}{ second file. }
  \item{from}{ first file. }
  \item{to}{ second file. }
  \item{overwrite}{ overwrite the destination file? }
  \item{copy.mode}{ logical: should file permission bits be copied where
    possible? This applies to both files and directories. }
  \item{paths}{ path to files. }
  \item{use_umask}{ use a mask? }
  \item{time}{ time to put on the file. }
  \item{force}{ force file deletion? }
  \item{header}{ header to use. }
  \item{title}{ title of the window. }
  \item{delete.file}{ delete the file once it is displayed? }
  \item{pager}{ pager to use to display this file. }
  \item{encoding}{ encoding to use for the file content. }
  \item{dir}{ directory to set. No change if it is \code{NULL} (by default). }
}

\details{
  TODO...
}

\value{
  TODO...
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, but these are indeed wrappers
  around existing functions written by the R Core Team in base or utils packages.
}

\seealso{ \code{\link{char}} }

\examples{
## TODO: various examples of dirs and files manipulation
}

\keyword{character}

\concept{ directories and files manipulation }
