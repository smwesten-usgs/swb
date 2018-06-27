############################################################################
# Matrix manipulation methods
############################################################################
# Converts an integer into a hexadecimal value
as.character.hexmode <- function(x) {
  hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
  isna <- is.na(x)
  y <- x[!isna]
  ans0 <- character(length(y))
  while (any(y > 0)) {
    z <- y%%16
    y <- floor(y/16)
    ans0 <- paste(hexDigit[z + 1], ans0, sep = "")
  }
  ans <- rep("NA", length(x))
  ans[!isna] <- ans0
  ans
}

############################################################################
# Matrix manipulation methods
############################################################################
# Flip matrix (upside-down)
flip.matrix <- function(x) {
  mirror.matrix(rotate180.matrix(x))
}

# Mirror matrix (left-right)
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Rotate matrix 90 clockworks
rotate90.matrix <- function(x) {
  t(mirror.matrix(x))
}

# Rotate matrix 180 clockworks
rotate180.matrix <- function(x) {
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

# Rotate matrix 270 clockworks
rotate270.matrix <- function(x) {
  mirror.matrix(t(x))
}

############################################################################
# Color methods
############################################################################
# The inverse function to col2rgb()
rgb2col <- function(rgb) {
  rgb <- as.integer(rgb)
  class(rgb) <- "hexmode"
  rgb <- as.character(rgb)
  rgb <- matrix(rgb, nrow=3)
  paste("#", apply(rgb, MARGIN=2, FUN=paste, collapse=""), sep="")
}

getColorTable <- function(col) {
  # Convert all colors into format "#rrggbb"
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}

############################################################################
# Draw methods
############################################################################
# 'colorTable' instead of 'col' to be more explicit.
image.matrix <- function(x, colorTable=NULL, xlab="", ylab="", ...) {
  image(x=1:ncol(x),y=1:nrow(x),z=rotate270.matrix(x), col=colorTable, xlab=xlab, ylab=ylab, ...)
}

############################################################################
# Example
############################################################################
#opar <- par(ask=TRUE)

#x <- y <- seq(-4 * pi, 4 * pi, len = 27)
#r <- sqrt(outer(x^2, y^2, "+"))
#z <- cos(r^2) * exp(-r/6)
#z[2,] <- min(z) # To show that z[1,1] is in the
#z[,2] <- max(z) # upper left corner.

#colorTable <- gray((0:32)/32)
#image.matrix(z, colorTable=colorTable)
#image.matrix(t(z), colorTable=colorTable)
#image.matrix(mirror.matrix(z), colorTable=colorTable)
#image.matrix(flip.matrix(z), colorTable=colorTable)

#img <- matrix("white", nrow=12, ncol=9)
#img[2:3,c(2:3,7:8)] <- "blue"
#img[5:6,5] <- "green"
#img[9,c(3,7)] <- "red"
#img[10,4:6] <- "red"

#colorTable <- getColorTable(img)
#z <- rgb2col(col2rgb(img)) # Assert format "#rrggbb"
#z <- match(z, colorTable)
#dim(z) <- dim(img)

#image.matrix(z, colorTable=colorTable)
#image.matrix(rotate90.matrix(z), colorTable=colorTable)

#par(opar)

# End of File