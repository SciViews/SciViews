## Essentially a series of base R function that manipulate files and directories
## and that are renamed/rationalized for facility
## TODO: is the object name correctly choosen? or filepath? or fpath?

## A replacement for file.path
filePath <- function (..., fsep = .Platform$file.sep)
{
	## Create a filePath objects inheriting from character
	return(structure(file.path(..., fsep = fsep),
		class = c("filePath", "character")))
}

## The print function of filename separates dirs (ending with /) from files
## and also indicate which file already exists on disk or not
## EXPERIMENTAL FEATURE... Should require an option to activate/inactivate
## test of files on disk!
## TODO: determine what is an alias or link!
print.filePath <- function (x, ...)
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

as.filePath <- function (x, ...)
	return(structure(as.character(x), class = c("filePath", "character")))

is.filePath <- function (x)
	return(inherits(x, "filePath"))

isDir <- function (filePath)
	return(file.info(filePath)$isdir)

isFile <- function (filePath)
	return(file.exists(filePath) & !file.info(filePath)$isdir)

## Rework paths
## basename
fileName <- function (filePath)
	return(structure(basename(filePath), class = c("filePath", "character")))

## dirname
fileDir <- function (filePath)
	return(structure(dirname(filePath), class = c("filePath", "character")))

## path.expand
fileExpand <- function (filePath)
	return(structure(path.expand(filePath), class = c("filePath", "character")))

## normalizePath
fileNormalize <- function (filePath, mustWork = FALSE)
	return(structure(normalizePath(filePath, winslash = "/", mustWork = mustWork),
		class = c("filePath", "character")))

## Get various files or directories
## R.home
dirR <- function (component = "home")
	return(structure(R.home(component), class = c("filePath", "character")))

## TODO: find.package() and path.package()	
## system.file TODO: case it returns ""! And should we use mustWork?
filePackage	<- function (..., package = "base", lib.loc = NULL, mustWork = FALSE)
	return(structure(system.file(..., package = package, lib.loc = lib.loc,
		mustWork = mustWork), class = c("filePath", "character")))
	
## tempdir
dirTemp <- function ()
	return(structure(tempdir(), class = c("filePath", "character")))

## tempfile
fileTemp <- function (pattern = "file", tmpdir = tempdir(), fileext = "")
	return(structure(tempfile(pattern = pattern, tmpdir = tmpdir,
		fileext = fileext), class = c("filePath", "character")))

## Sys.which, TODO: keep names and display them in print.filePath objects!
fileFind <- function (names)
	return(structure(Sys.which(names), names = names, class = c("filePath", "character")))

## List dirs = dir() = list.dirs()
dirList <- function (filePath = ".", full.names = TRUE, recursive = TRUE)
	return(structure(list.dirs(path = filePath, full.names = full.names,
		recursive = recursive), class = c("filePath", "character")))

## List files = dir() and list.files()
fileList <- function (filePath = ".", pattern = NULL, all.files = FALSE,
full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
	return(structure(dir(path = filePath, pattern = pattern, all.files = all.files,
		full.names = full.names, recursive = recursive,
		ignore.case = ignore.case, include.dirs = include.dirs),
		class = c("filePath", "character")))

## List files using wildcard expansion ('globbing')
fileListGlob <- function (filePath, dir.mark = FALSE)
	return(structure(Sys.glob(paths = filePath, dirmark = dir.mark),
		class = c("filePath", "character")))

## Various file manipulation functions that do not return a path object
## (just homogenize the name...)
dirCreate <- .Recode(get("dir.create", envir = baseenv()))
fileAccess <- .Recode(get("file.access", envir = baseenv()))
fileAppend <- .Recode(get("file.append", envir = baseenv()))
fileRename <- .Recode(get("file.rename", envir = baseenv()))
fileCopy <-	.Recode(get("file.copy", envir = baseenv()))
fileCreate <- .Recode(get("file.create", envir = baseenv()))
fileExists <- .Recode(get("file.exists", envir = baseenv()))
fileInfo <-	.Recode(get("file.info", envir = baseenv()))
fileChmod <- .Recode(get("Sys.chmod", envir = baseenv()))
fileUMask <- .Recode(get("Sys.umask", envir = baseenv()))
fileTime <- function (filePath, time)
	Sys.setFileTime(path = filePath, time = time)
fileRemove <- .Recode(get("file.remove", envir = baseenv()))
## This is "stronger" than fileRemove()!
fileDelete <- function (filePath, recursive = FALSE, force = FALSE)
	return(unlink(x = filePath, recursive = recursive, force = force))

fileLink <- .Recode(get("file.link", envir = baseenv()))
fileSymLink <- .Recode(get("file.symlink", envir = baseenv()))
fileReadLink <- function (filePath)
	return(structure(Sys.readlink(paths = filePath),
		class = c("filePath", "character")))

## This is linked to some GUI element, possibly... anyway...
fileShow <-	.Recode(get("file.show", envir = baseenv()))
## TODO: this file choose... but this is really for svDialogs (dlgOpen(), dlgSave())
#fileChoose <- file.choose

## A more convenient setwd()/getwd() using objects
wdir <- function (dir = NULL)
{
	if (is.null(dir)) {
		dir <- getwd()
		class(dir) <- c("filePath", "character")
		## Make sure to use /, even under Windows
		dir <- gsub("\\\\", "/", dir)
		return(dir)
	} else { # Change current working directory
		owdir <- setwd(dir)
		## Make sure to use /, even under Windows
		owdir <- gsub("\\\\", "/", owdir)
		class(owdir) <- c("filePath", "character")
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
		class(dir) <- c("filePath", "character")
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
		class(osdir) <- c("filePath", "character")
		## Save old session directory
		.assignTemp(".osdir", osdir)
		return(osdir)
	}
}
