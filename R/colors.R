#' Various color palettes.
#'
#' Create vectors of `n` contiguous colors.
#'
#' @param n The number of colors (>= 1) to be in the palette.
#' @param alpha The alpha transparency, a number in \[0, 1\], see argument
#' `alpha =` in [hsv()].
#' @param s The 'saturation' to be used to complete the HSV color descriptions.
#' @param v The 'value' to use for the HSV color descriptions.
#' @details `cwm_colors(s = 0.5, v = 1)` gives very similar colors to
#' `cm.colors()`.
#' `ryg_colors()` is similar to `rainbow(start = 0, end = 2/6)`.
#' The `xxx_colors()` (tidyverse name-compatible) and `xxx.colors()``
#' (grDevices name-compatible) functions are synonyms.
#' @export
#' @name colors
#' @seealso [cm.colors()], [colorRampPalette()]
#' @keywords color
#' @concept color palettes
#' @examples
#' # Draw color wheels with various palettes
#' opar <- par(mfrow = c(2, 2))
#' pie(rep(1, 11), col = cwm.colors(11), main = "Cyan - white - magenta")
#' pie(rep(1, 11), col = rwb.colors(11), main = "Red - white - blue")
#' pie(rep(1, 11), col = rwg.colors(11), main = "Red - white - green")
#' pie(rep(1, 11), col = ryg.colors(11), main = "Red - yellow - green")
#' par(opar)
rwb_colors <- function(n, alpha = 1, s = 0.9, v = 0.9) {
  if ((n <- as.integer(n[1L])) <= 0) return(character(0L))
  # Define the initial (red) and final (blue) colors with white in between
  cols <- c(hsv(0, s, v, alpha = alpha),   # Red
            hsv(0, 0, v, alpha = alpha),   # White
            hsv(2/3, s, v, alpha = alpha)) # Blue
  # Use a color ramp from red to white to blue
  colorRampPalette(cols)(n)
}

#' @export
#' @rdname colors
rwb.colors <- rwb_colors # grDevices compatibility

#' @export
#' @rdname colors
rwg_colors <- function(n, alpha = 1, s = 0.9, v = 0.9) {
  if ((n <- as.integer(n[1L])) <= 0) return(character(0L))
  # Define the initial (red) and final (blue) colors with white in between
  cols <- c(hsv(0, s, v, alpha = alpha),   # Red
            hsv(0, 0, v, alpha = alpha),   # White
            hsv(2/6, s, v, alpha = alpha)) # Green
  # Use a color ramp from red to white to green
  colorRampPalette(cols)(n)
}

#' @export
#' @rdname colors
rwg.colors <- rwg_colors # grDevices compatibility

#' @export
#' @rdname colors
ryg_colors <- function(n, alpha = 1, s = 0.9, v = 0.9) {
  # This is essentially rainbow(), but going from 0 (red) to 2/6 (green)
  rainbow(n, s = s, v = v, start = 0, end = 2/6, alpha = alpha)
}

#' @export
#' @rdname colors
ryg.colors <- ryg_colors # grDevices compatibility

#' @export
#' @rdname colors
cwm_colors <- function(n, alpha = 1, s = 0.9, v = 0.9) {
  # Slighly different than cm.colors(), allowing for s and v!
  # Produce probably better results on a CMYK device (color printer)?
  if ((n <- as.integer(n[1L])) <= 0) return(character(0L))
  # Define the initial (red) and final (blue) colors with white in between
  cols <- c(hsv(1/2, s, v, alpha = alpha), # Cyan
            hsv(0, 0, v, alpha = alpha),   # White
            hsv(5/6, s, v, alpha = alpha)) # Magenta
  # Use a color ramp from cyan to white to magenta
  colorRampPalette(cols)(n)
}

#' @export
#' @rdname colors
cwm.colors <- cwm_colors # grDevices compatibility
