## Essentially a series of base R function that manipulate files and directories
## and that are renamed/rationalized for facility

## A replacement for file.path
path <- function (..., fsep = .Platform$file.sep)
{
	## Create a path objects inheriting from character
	return(structure(file.path(..., fsep = fsep),
		class = c("path", "character")))
}

## The print function of filename separates dirs (ending with /) from files
## and also indicate which file already exists on disk or not
## EXPRERIMENTAL FEATURE... Should require an option to activate/inactivate
## test of files on disk!
print.path <- function (x, ...)
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

as.path <- function (x, ...)
	return(structure(as.character(x), class = c("path", "character")))

is.path <- function (x)
	return(inherits(x, "path"))

isDir <- function (path)
	return(file.info(path)$isdir)

isFile <- function (path)
	return(file.exists(path) & !file.info(path)$isdir)

## Rework paths
## basename
fileName <- function (path)
	return(structure(basename(path), class = c("path", "character")))

## dirname
fileDir <- function (path)
	return(structure(dirname(path), class = c("path", "character")))

## path.expand
fileExpand <- function (path)
	return(structure(path.expand(path), class = c("path", "character")))

## normalizePath
fileNormalize <- function (path, mustWork = FALSE)
	return(structure(normalizePath(path, winslash = "/", mustWork = mustWork),
		class = c("path", "character")))

## Get various files or directories
## R.home
dirR <- function (component = "home")
	return(structure(R.home(component), class = c("path", "character")))

## TODO: find.package() and path.package()	
## system.file TODO: case it returns ""! And should we use mustWork?
filePackage	<- function (..., package = "base", lib.loc = NULL, mustWork = FALSE)
	return(structure(system.file(..., package = package, lib.loc = lib.loc,
		mustWork = mustWork), class = c("path", "character")))
	
## tempdir
dirTemp <- function ()
	return(structure(tempdir(), class = c("path", "character")))

## tempfile
fileTemp <- function (pattern = "file", tmpdir = tempdir(), fileext = "")
	return(structure(tempfile(pattern = pattern, tmpdir = tmpdir,
		fileext = fileext), class = c("path", "character")))

## Sys.which, TODO: keep names and display them in print.path objects!
fileFind <- function (names)
	return(structure(Sys.which(names), names = names, class = c("path", "character")))

## List dirs = dir() = list.dirs()
dirList <- function (path = ".", full.names = TRUE, recursive = TRUE)
	return(structure(list.dirs(path = path, full.names = full.names,
		recursive = recursive), class = c("path", "character")))

## List files = dir() and list.files()
fileList <- function (path = ".", pattern = NULL, all.files = FALSE,
full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
	return(structure(dir(path = path, pattern = pattern, all.files = all.files,
		full.names = full.names, recursive = recursive,
		ignore.case = ignore.case, include.dirs = include.dirs),
		class = c("path", "character")))

## List files using wildcard expansion ('globbing')
fileListGlob <- function (path, dir.mark = FALSE)
	return(structure(Sys.glob(paths = path, dirmark = dir.mark),
		class = c("path", "character")))

## Various file manipulation functions that do not return a path object
## (just homogenize the name...)
dirCreate <- get("dir.create", envir = baseenv())
fileAccess <- get("file.access", envir = baseenv())
fileAppend <- get("file.append", envir = baseenv())
fileRename <- get("file.rename", envir = baseenv())
fileCopy <-	get("file.copy", envir = baseenv())
fileCreate <- get("file.create", envir = baseenv())
fileExists <- get("file.exists", envir = baseenv())
fileInfo <-	get("file.info", envir = baseenv())
fileChmod <- get("Sys.chmod", envir = baseenv())
fileRemove <- get("file.remove", envir = baseenv())
## This is "stronger" than fileRemove()!
fileDelete <- function (path, recursive = FALSE, force = FALSE)
	return(unlink(x = path, recursive = recursive, force = force))

fileLink <- get("file.link", envir = baseenv())
fileSymlink <- get("file.symlink", envir = baseenv())
fileReadLink <- function (path)
	return(structure(Sys.readlink(paths = path),
		class = c("path", "character")))

## This is linked to some GUI element, possibly... anyway...
fileShow <-	get("file.show", envir = baseenv())
## TODO: this file choose... but this is really for svDialogs (dlgOpen(), dlgSave())
#fileChoose <- file.choose

## A more convenient setwd()/getwd() using objects
wdir <- function (dir = NULL)
{
	if (is.null(dir)) {
		dir <- getwd()
		class(dir) <- c("path", "character")
		## Make sure to use /, even under Windows
		dir <- gsub("\\\\", "/", dir)
		return(dir)
	} else { # Change current working directory
		owdir <- setwd(dir)
		## Make sure to use /, even under Windows
		owdir <- gsub("\\\\", "/", owdir)
		class(owdir) <- c("path", "character")
		## Save old working directory
		.assignTemp(".owdir", owdir)
		return(owdir)
	}
}

## Get or set session dir
sdir <- function (dir = NULL)
{
	if (is.null(dir)) {
		dir <- getOption("R.initdir")
		if (is.null(dir)) return(NULL)
		class(dir) <- c("path", "character")
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
		class(osdir) <- c("path", "character")
		## Save old session directory
		.assignTemp(".osdir", osdir)
		return(osdir)
	}
}
