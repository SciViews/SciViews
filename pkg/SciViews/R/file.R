## Essentially a series of base R function that manipulate files and directories
## and that are renamed/rationalized for facility

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

## A replacement for file.path
filePath <- function (..., fsep = .Platform$file.sep)
{
	## Create a filePath objects inheriting from character
	return(structure(.Internal(file.path(list(...), fsep)),
		class = c("filePath", "character")))
}

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

## Rework file paths
## basename
fileName <- function (path)
	return(structure(basename(path), class = c("filePath", "character")))

## dirname
fileDir <- function (path)
	return(structure(dirname(path), class = c("filePath", "character")))

## path.expand
fileExpand <- function (path)
	return(structure(path.expand(path), class = c("filePath", "character")))

## normalizePath
fileNormalize <- function (path, mustWork = FALSE)
	return(structure(normalizePath(path, winslash = "/", mustWork = mustWork),
		class = c("filePath", "character")))

## Get various files or directories
## R.home
dirR <- function (component = "home")
	return(structure(R.home(component), class = c("filePath", "character")))
	
## system.file TODO: case it returns ""! And should we use mustWork?
filePackage	<- function (..., package = "base", lib.loc = NULL, mustWork = FALSE)
	return(structure(system.file(..., package = package, lib.loc = lib.loc,
		mustWork = mustWork), class = c("filePath", "character")))
	
## tempdir
dirTemp <- function ()
	return(structure(.Internal(tempdir()), class = c("filePath", "character")))

## tempfile
fileTemp <- function (pattern = "file", tmpdir = tempdir(), fileext = "")
	return(structure(.Internal(tempfile(pattern, tmpdir, fileext)),
		class = c("filePath", "character")))

## Sys.which, TODO: keep names and display them in print.filePath objects!
fileFind <- function (names)
	return(structure(Sys.which(names), class = c("filePath", "character")))


#dirList <- dir
#dirList <- list.dirs
#fileList <-	list.files
#dirCreate <- dir.create
#fileAccess <- file.access
#fileAppend <- file.append
#fileChoose <- file.choose
#fileCopy <-	file.copy
#fileCreate <- file.create
#fileExists <- file.exists
#fileInfo <-	file.info
#fileLink <- file.link
#fileRemove <- file.remove
#fileRename <- file.rename
#fileShow <-	file.show
#fileSymlink <- file.symlink
#fileChmod <- Sys.chmod
#fileGlob <- Sys.glob
#fileUnlink <- unlink
# = isDir/isFile

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
		.osdir <<- osdir
		return(osdir)
	}
}
