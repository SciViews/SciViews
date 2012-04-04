.onLoad <- function (lib, pkg)
{
	## TODO: check configuration and install everything that we need to use the
	## SciViews extensions, including the HTTP or socket server
	#serve <- getOption("ko.serve")
	#if (!is.null(serve)) {
	#	startSocketServer(port = as.integer(serve)[1])
	#	guiInstall()
	#}
}

.onUnload <- function (libpath)
{
	#serve <- getOption("ko.serve")
	#if (!is.null(serve) && "package:svSocket" %in% search())
	#	stopSocketServer(port = as.integer(serve)[1])
	#guiUninstall()
}

.packageName <- "SciViews"

## Rethink this first before making this public
.subclass <- function (x, class, superclasses = NULL)
{
	## TODO: check this is an S3 object that inherits from the given class(es)
	if (!is.null(superclasses)) {
		misClass <- inherits(x, as.character(superclasses), which = TRUE) == 0
		if (any(misClass))
			stop("'x' does not inherits from", paste(superclasses[misClass],
				collapse = ", "))
	}
	## Check if new class in not already defined
	if (class %in% class(x)) return(x)
	## Prepend that class
	class(x) <- c(class, class(x))
	return(x)
}

`.subclass<-` <- function (x, value)
{
	if (!value %in% class(x)) class(x) <- c(value, class(x))
	return(x)
}

## Code borrowed from svMisc, to avoid a dependency!
.TempEnv <- function ()
{
    pos <-  match("TempEnv", search())
    if (is.na(pos)) { # Must create it
        TempEnv <- list()
        attach(TempEnv, pos = length(search()) - 1)
        rm(TempEnv)
        pos <- match("TempEnv", search())
    }
    return(pos.to.env(pos))
}

.assignTemp <- function (x, value, replace.existing = TRUE)
    if (isTRUE(replace.existing) || !exists(x, envir = .TempEnv(), mode = "any",
		inherits = FALSE))
        assign(x, value, envir = .TempEnv())

## This is for convenience: . == .GlobalEnv
.assignTemp(".", base::.GlobalEnv)

