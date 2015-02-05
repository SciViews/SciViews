## Package graphics: renaming of functions to meet SciViews coding convention
## and definition of a couple new functions
## Must add things from plotrix!!! => svPlot + vioplot + wvioplot + beanplot + ellipse + gplots

## Graphical options
## par(.., no.readonly = FALSE) is not explicit enough
## Covered functions: graphics::par()
plotOpt <- function (...) graphics::par(..., no.readonly = TRUE)
plotOptAll <- function (...) graphics::par(..., no.readonly = FALSE)
#clip() # Set clipping region in the graph

## The plot.xxx() functions...
## Covered functions: graphics::plot.new(), graphics::frame()
plotNew <- .Recode(graphics::plot.new) # () and synonym to frame() that is ambiguous => don't use it

## The simple dividing of equidistant boxes is done using plotOpt(mfrow) & plotOpt(mfcol)

## The layout() mechanism
## Covered functions: graphics::layout(), graphics::layout.show()
#layout()
layoutShow <- graphics::layout.show

## The screen() mechanism
## Covered functions: graphics::split.screen(), graphics::screen()
## graphics::erase.screen(), graphics::close.screen()
screenSplit <- graphics::split.screen
screenSet <- graphics::screen
screenDelete <- graphics::erase.screen
screenClose <- graphics::close.screen

## Dimensions
## Covered functions: graphics::lcm(), graphics::xinch(), graphics::yinch(),
## graphics::xyinch(), graphics::grconvertX(), graphics::grconvertY()
#lcm() # Take a number "x" and returns a string with "x cm"
# Note: __ is used to denote the units, like lenght__cm
l__cm <- graphics::lcm
#xinch(), yinch() and xyinch() convert from inch to plot units
x__in2user <- graphics::xinch
y__in2user <- graphics::yinch
xy__in2user <- graphics::xyinch
#Same for cm2user
x__cm2user <- function (x = 1, warn.log = TRUE)
	x__in2user(x = x / 2.54, warn.log = warn.log)
y__cm2user <- function (y = 1, warn.log = TRUE)
	y__in2user(y = y / 2.54, warn.log = warn.log)
xy__cm2user <- function (xy = 1, warn.log = TRUE)
	xy__in2user(xy = xy / 2.54, warn.log = warn.log)
## grconvertX() and grconvertY() do not allow metric units,
## plus there is no grcxonvertXY()
## TODO: eliminate mm here!
xConvert <- function (x, from = "user", to = "user")
{
	## Perform conversion from cm or mm to inches in from, if needed
	x <- switch(from,
		cm = x / 2.54,
		mm = x / 25.4,
		x)
	if (from %in% c("cm", "mm")) from <- "inches"
	if (to %in% c("cm", "mm")) {
		res <- graphics::grconvertX(x = x, from = from, to = "inches")
		if (to == "cm") res <- res * 2.54 else res <- res * 25.4
		return(res)
	} else return(graphics::grconvertX(x = x, from = from, to = to))
}

yConvert <- function (y, from = "user", to = "user")
{
	## Perform conversion from cm or mm to inches in from, if needed
	y <- switch(from,
		cm = y / 2.54,
		mm = y / 25.4,
		y)
	if (from %in% c("cm", "mm")) from <- "inches"
	if (to %in% c("cm", "mm")) {
		res <- graphics::grconvertY(y = y, from = from, to = "inches")
		if (to == "cm") res <- res * 2.54 else res <- res * 25.4
		return(res)
	} else return(graphics::grconvertY(y = y, from = from, to = to))
}

xyConvert <- function (x, y, from = "user", to = "user")
{
	## Either x and y are provided, or x is a list with $x and $y
	if (missing(y)) {
		if (!is.list(x) && names(x) != c("x", "y"))
			stop("You must provide a list with 'x' and 'y', or two vectors")
		y <- x$y
		x <- x$x
	}
	return(list(
		x = xConvert(x = x, from = from, to = to),
		y = yConvert(y = y, from = from, to = to)))
}

## Basic drawing functions in the graphics package
#points() # generic
#lines() # generic
#matpoints()
#matlines()
#abline()
#segments()
#arrows()
#polygon()
#polypath()
#curve()
#rect()
## TODO: add circle() and ellipse()??? from ellipse package!
## ellipse there is a generic function to draw confidence region for two parameters
## It is: ellipse <- function (x, ...) UseMethod("ellipse")
## => use ovals() and circles() instead???
## There are draw.circle() and draw.ellipse() in the plotrix package
#box()
# axis() and Axis() as an exception!
#axTicks() in graphics versus axisTicks() in grDevices => forgot about the former?
#grid() # TODO: rename this to avoid confusion with the grid graphic system?
#rug()
#text() # generic
#mtext()
#title()
#legend()
# Note: graphics::strwidth() and graphics::strheight() are treated in character.R!
#symbols()
#rasterImage() # Leave like it is?
## Problem with contour() # generic, add items to a graph when using add = TRUE
## We want the same mechanisms as for plot() vs points()/lines()... So, here,
## we must redefine a generic for that!
## We use contours() vs contourplot()
contours <- function (x, ...) UseMethod("contours")
contours.default <- function (x = seq(0, 1, length.out = nrow(z)),
y = seq(0, 1, length.out = ncol(z)), z, labels = NULL, labcex = 0.6,
drawlabels = TRUE, method = "flattest", col = par("fg"), lty = par("lty"),
lwd = par("lwd"), add = TRUE, ...)
	graphics::contour.default(x = x, y = y, z = z, labels = labels,
		labcex = labcex, drawlabels = drawlabels, method = method, col = col,
		lty = lty, lwd = lwd, add = add, ...)
## See also the shape package!
## qqlines() from stats
## plot() method of density object in stats + plot() method of hclust objects

## High-level plot function in the graphics package
## Covered functions: graphics::filled.contour(), graphics::dotchart(),
## graphics::smoothScatter(), graphics::stars(), graphics::stem(),
## graphics::stripchart(), graphics::contour()
#plot() # generic
#hist() # generic
#pie() # Exception, because pieplot() does not exist in English
#image() # generic # Exception: instead of imageplot() or so?
#   + heat.colors(), terrain.colors() and topo.colors() from grDevices!
#pairs() # generic # TODO: define pairsplot()?
#persp() # generic # TODO: define persplot()? + perspx() in plotrix!?
#matplot()
#barplot() # generic
#boxplot() # generic
#contour() # generic, create a graph when using default add = FALSE; contourplot() in lattice!
contourplot <- graphics::contour
## R CMD check claims he does not find filledcontour => we don't copy the
## function but call it from filledplot()
filledplot <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
col = color.palette(length(levels) - 1), plot.title, plot.axes, 
key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
axes = TRUE, frame.plot = axes, ...)
	graphics::filled.contour(x = x, y = y, z = z, xlim = xlim, ylim = ylim,
		zlim = zlim, levels = levels, nlevels = nlevels,
		color.palette = color.palette, col = col, plot.title = plot.title,
		plot.axes = plot.axes, key.title = key.title, key.axes = key.axes,
		asp = asp, xaxs = xaxs, yaxs, yaxs, las = las, axes = axes,
		frame.plot = frame.plot, ...)
starplot <- graphics::stars
stemplot <- function (x, scale = 1, width = 80, atom = 1e-08)
	graphics::stem(x = x, scale = scale, width = width, atom = atom)
stripplot <- graphics::stripchart
clevelandplot <- graphics::dotchart
smoothplot <- graphics::smoothScatter
#coplot()
#fourfoldplot()
#cdplot() # generic
#mosaicplot() # generic
#spineplot() # generic
#sunflowerplot() # generic
#assocplot() with a better version in vcd as assoc()!
#+vectorplot() in vectorplot.R!
## + qqplot()/qqnorm() from stats
## + screeplot() + biplot()

## Panel functions and other utilities
## Problem: panel is used in lattice => rename this here?
## Covered functions: graphics::panel.smooth(), graphics::co.intervals()
## + panel.hist() and panel.cor() in ?pairs example
## one way to differentiate them, is to use panel at the end here, like plot
smoothPanel <- graphics::panel.smooth
## TODO: rework the various panel.XXX function in panels.R, panels.diag.R & pcomp.R!
coplotIntervals <- graphics::co.intervals

## Graphic interaction in the graphics package
#locator()
#identify() # generic

## "Internal" function in graphics package (normally not for the end-user)
# Not normally called by the end-user
## Covered functions: graphics::plot.window(), graphics::plot.xy(),
## graphics::.filled.contour(), graphics::bxp(), 
plotWindowInternal <- .Recode(graphics::plot.window)
plotInternal <- .Recode(graphics::plot.xy)
boxplotInternal <- graphics::bxp
## Apparently not in R 2.14.0!
#filledplotInternal <- graphics::.filled.contour


## grDevices ###################################################################

## Devices management
devNew <- grDevices::dev.new
devCur <- .Recode(grDevices::dev.cur)
devList <- grDevices::dev.list
devNext <- .Recode(grDevices::dev.next); formals(devNext)$which <- quote(devCur())
devPrev <- .Recode(grDevices::dev.prev); formals(devPrev)$which <- quote(devCur())
devSet <- .Recode(grDevices::dev.set); formals(devSet)$which <- quote(devNext())
devClose <- .Recode(grDevices::dev.off); formals(devClose)$which <- quote(devCur())
devCloseAll <- grDevices::graphics.off
devControl <- .Recode(grDevices::dev.control)

## The following two functions call .Internal(devHoldFlush) and here R CMD check
## got fooled because it does not found a function called devHoldFlush()
## So, we define also a devHoldFlush() function here to cope with this problem
devHold <- .Recode(grDevices::dev.hold)
devFlush <- .Recode(grDevices::dev.flush)
devHoldFlush <- function (level = 1L)
{
	level <- round(level)[1]
	if (level > 0) {
		devHold(level)
	} else if (level < 0) {
		devFlush(-level)
	} else return()
}
devCopy <- .Recode(grDevices::dev.copy)
devCopyNew <- grDevices::dev.print
devCopyEps <- grDevices::dev.copy2eps
devCopyPdf <- grDevices::dev.copy2pdf
devCopyBitmap <- grDevices::dev2bitmap
devSave <- .Recode(grDevices::savePlot)
## Because these two function call .Internal() that fools R CMD check, we define
## getSnapshot() and playSnapshot() as synonyms of devRecord() and devReplay()
devRecord <- getSnapshot <- .Recode(grDevices::recordPlot)
devReplay <- playSnapshot <- .Recode(grDevices::replayPlot)
devCapture <- .Recode(grDevices::dev.capture)
#devAskNewPage()
## For devSize, default unit is "cm" instead of "in" for dev.size()
devSize <- .Recode(grDevices::dev.size); formals(devSize)$units <- c("cm", "in", "px")
devCapabilities <- .Recode(grDevices::dev.capabilities)
devInteractive <- grDevices::dev.interactive
isDevInteractive <- grDevices::deviceIsInteractive

## TODO: a way to define these functions as platform independent!
## Graphic devices
#if (.Platform$OS.type == "unix") {
#	devX11 <- grDevices::X11 # + x11()
#	devX11Opt <- grDevices::X11.options
#}
#if (grepl("^mac", .Platform$pkgType)) {
#	devQuartz <- grDevices::quartz
#	devQuartzOpt <- grDevices::quartz.options
#	## There is a quartz.save() function defined somewhere!
#}
#if (.Platform$OS.type == "windows") {
#	devWin <- grDevices::windows
#	devWinOpt <- grDevices::windows.options
#	devWinPrint <- grDevices::win.print
#	devWinMetafile <- grDevices::win.metafile
#	devToTop <- grDevices::bringToTop # TODO: a similar function for Linux and Mac OS X!
#	formals(devToTop)$which <- quote(devCur())
#	# this is bringToTop(which = dev.cur(), stay = FALSE) # with -1 is console
#	devMsg <- grDevices::msgWindow
#	formals(devMsg)$which <- quote(devCur()) # TODO: a similar function for Linux and Mac OS X
#	# this is msgWindow(type = c("minimize", "restore", "maximize", "hide", "recordOn", "recordOff"),
#    #	which = dev.cur()
#	#recordGraphics(expr, list, env) # A function intended *only* for experts
#}
devPdf <- grDevices::pdf
devPdfOpt <- grDevices::pdf.options
devPS <- grDevices::postscript
devPSOpt <- grDevices::ps.options
#setEPS()
#setPS()
devPdfCairo <- grDevices::cairo_pdf
devPSCairo <- grDevices::cairo_ps
devSvg <- grDevices::svg
devBitmap <- grDevices::bitmap
devXfig <- grDevices::xfig

## The following four functions call .Internal(X11) which has more arguments
## than X11 itself => we don't copy content, but call it instead
devBmp <- function (filename = "Rplot%03d.bmp", width = 480, height = 480, 
units = "px", pointsize = 12, bg = "white", res = NA, ..., 
type = c("cairo", "Xlib", "quartz"), antialias) 
	grDevices::bmp(filename = filename, width = width, height = height,
		units = units, pointsize = pointsize, bg = bg, res = res, ...,
		type = type, antialias = antialias)
devJpeg <- function (filename = "Rplot%03d.jpeg", width = 480, height = 480, 
units = "px", pointsize = 12, quality = 75, bg = "white", 
res = NA, ..., type = c("cairo", "Xlib", "quartz"), antialias)
	grDevices::jpeg(filename = filename, width = width, height = height,
		units = units, pointsize = pointsize, quality = quality, bg = bg,
		res = res, ..., type = type, antialias = antialias)
devPng <- function (filename = "Rplot%03d.png", width = 480, height = 480, 
units = "px", pointsize = 12, bg = "white", res = NA, ..., 
type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
	grDevices::png(filename = filename, width = width, height = height,
		units = units, pointsize = pointsize, bg = bg, res = res, ...,
		type = type, antialias = antialias)
devTiff <- function (filename = "Rplot%03d.tiff", width = 480, height = 480, 
units = "px", pointsize = 12, compression = c("none", "rle", 
"lzw", "jpeg", "zip"), bg = "white", res = NA, ..., type = c("cairo", 
"Xlib", "quartz"), antialias)
	grDevices::tiff(filename = filename, width = width, height = height,
		units = units, pointsize = pointsize, compression = compression,
		bg = bg, res = res, ..., type = type, antialias = antialias)
#pictex() # device, historical interest only

## Color management
#palette() # get or set the color palette
#colors() and colours() for a list of color names
## TODO: wrong name? colorToRgb?
color2rgb <- .Recode(grDevices::col2rgb) # convert colors to rgb
#rgb()
#rgb2hsv()
#hsv()
#hcl()
#gray() and grey()
colorAdjust <- grDevices::adjustcolor
#colorRamp() and colorRampPalette() to create color ramps
colorDens <- grDevices::densCols
## Predefined color sets
colorBlues9 <- function () grDevices::blues9
colorRainbow <- grDevices::rainbow
colorHeat <- grDevices::heat.colors
colorTerrain <- grDevices::terrain.colors
colorTopo <- grDevices::topo.colors
colorCm <- grDevices::cm.colors
colorCwm <- cwm.colors
colorRwb <- rwb.colors
colorRyg <- ryg.colors
colorGray <- grDevices::gray.colors
colorGrey <- grDevices::grey.colors
## colorConverter object
#colorConverter()
colorConvertRgb <- grDevices::make.rgb
colorConvert <- grDevices::convertColor

## Fonts
fontType1 <- grDevices::Type1Font
fontCid <- grDevices::CIDFont
fontsPS <- grDevices::postscriptFonts
fontsPdf <- grDevices::pdfFonts
fontsEmbed <- grDevices::embedFonts
#if (.Platform$OS.type == "windows") {
#	#windowsFont()
#	#windowsFonts()
#}
#if (grepl("^mac", .Platform$pkgType)) {
#	#quartzFont()
#	#quartzFonts()
#}
#if (.Platform$OS.type == "unix") {
#	#X11Font()
#	#X11Fonts()
#}

## raster objects
## TODO: a raster() function to create such an object
#as.raster()
#is.raster()
#+ rasterImage() in the package graphics to draw such a raster object in a plot

## Graphic events
## TODO: change to: devEvent, devEventHandlers, devEventEnv, devEventEnv<-
#getGraphicsEvent()
#setGraphicsEventHandlers()
#getGraphicsEventEnv()
#setGraphicsEventEnv()

## Graphic annotations
#as.graphicsAnnot()

## Utility functions
optCheck <- grDevices::check.options #utility function to check options consistency!
nclassSturges <- grDevices::nclass.Sturges
nclassScott <- grDevices::nclass.scott
nclassFD <- grDevices::nclass.FD
#chull()
#contourLines()
#trans3d() # transform from 3d to 2d
rangeExtend <- grDevices::extendrange
#pretty() from base => rangePretty()? but a generic function!
#axisTicks(), .axisPars()
boxplotStats <- grDevices::boxplot.stats
#xyTable() # Used by sunflowerplot()
xyCoords <- grDevices::xy.coords
xyzCoords <- grDevices::xyz.coords
in2cm <- grDevices::cm
cm2in <- function (x) x / cm(1)
#n2mfrow() computes sensible mfrow from number of graphs
# + .ps.prolog

## Dynamite plot by Samuel Brown
## http://www.r-bloggers.com/dynamite-plots-in-r/
## Much critisize! See http://emdbolker.wikidot.com/blog%3Adynamite
## http://pablomarin-garcia.blogspot.co.nz/2010/02/why-dynamite-plots-are-bad.html
## http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiKoyama/Poster3.pdf
## TODO: find a better representation for ANOVE; al least both hgh ad low wiskers
## or superpose points, or vioplot, or...?
#dynamitePlot <- function (height, error, names = NA, significance = NA,
#ylim = c(0, maxLim), ...)
#{
#    maxLim <- 1.1 * max(mapply(sum, height, error))
#    bp <- barplot(height, names.arg = names, ylim = ylim, ...)
#    arrows(x0 = bp, y0 = height, y1 = height + error, angle = 90)
#    text(x = bp, y = 0.2 + height + error, labels = significance)
#}
#Values <- c(1, 2, 5, 4)
#Errors <- c(0.25, 0.5, 0.33, 0.12)
#Names <- paste("Trial", 1:4)
#Sig <- c("a", "a", "b", "b")
#dynamitePlot(Values, Errors, names = Names, significance = Sig)
