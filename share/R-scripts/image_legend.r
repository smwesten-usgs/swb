image.scale <-
function (z, col, x, y = NULL, size = NULL, digits = 2, labels = c("breaks",
    "ranges"))
{
    # sort out the location
    n <- length(col)
    usr <- par("usr")
    mx <- mean(usr[1:2]); my <- mean(usr[3:4])
    dx <- diff(usr[1:2]); dy <- diff(usr[3:4])
    if (missing(x))
        x <- mx + 1.05*dx/2 # default x to right of image
    else if (is.list(x)) {
        if (length(x$x) == 2)
          size <- c(diff(x$x), -diff(x$y)/n)
        y <- x$y[1]
        x <- x$x[1]
    } else x <- x[1]
    if (is.null(size))
        if (is.null(y)) {
          size <- 0.618*dy/n # default size, golden ratio
          y <- my + 0.618*dy/2 # default y to give centred scale
        } else size <- (y-my)*2/n
    if (length(size)==1)
        size <- rep(size, 2) # default square boxes
    if (is.null(y))
        y <- my + n*size[2]/2
    # draw the image scale
    i <- seq(along = col)
    rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2],
        col = rev(col), xpd = TRUE)
    # sort out the labels
    rng <- range(z, na.rm = TRUE)
    bks <- seq(from = rng[2], to = rng[1], length = n + 1)
    bks <- formatC(bks, format="f", digits=digits)
    labels <- match.arg(labels)
    if (labels == "breaks")
        ypts <- y - c(0, i) * size[2]
    else {
        bks <- paste(bks[-1], bks[-(n+1)], sep = " to ")
        ypts <- y - (i - 0.5) * size[2]
    }
    text(x = x + 1.2 * size[1], y = ypts, labels = bks, adj =
        ifelse(size[1]>0, 0, 1), xpd = TRUE)
}

#\name{image.scale}

#\alias{image.scale}

#\title{Provide scale to image plots}

#\usage{
#image.scale(z, col, x, y=NULL, size=NULL, digits=2, labels=c("breaks", "ranges"))
#}

#\arguments{
# \item{z}{Data from image plot}
# \item{col}{Colours from image plot}
# \item{x}{Horizintal location of top-left corner of scale, or list with
#\code{x} and \code{y} components}
# \item{y}{Vertical location of top-left corner of scale}
# \item{size}{1- or 2-vector of colour-box dimensions}
# \item{digits}{Number of digits after the decimal point in labels}
# \item{labels}{Type of labels}
#}

#\description{
#Provides a vertical colour scale to accompany an image plot. The
#location defaults to the right of the plot, the colour-boxes
#default to square, and the style of the labels defaults to giving
#the breaks to the right of the scale.}

#\details{
#Use \code{x=locator(1)} or give both \code{x} and \code{y}
#arguments to specify the top-left corner of the scale. The
#colour-boxes then default to squares, and the image is centred
#around the vertical midpoint. Use \code{x=locator(2)} for
#complete control of the scale size and location. The usual scale
#(labels to the right) requires a top-left and bottom-right. To
#reverse the scale, go bottom-top. To swith labels to the left,
#go right-left.

#The labels default to single values giving the breaks, centred
#between colour-boxes. For ranges centred vertically on each
#colour-box (wider), specify \code{labels="ranges"}.}
#
#\author{Jonathan Rougier}

#\seealso{\code{\link{image}}}
#
#\examples{
# create an image plot
#x <- seq(-0.5, 0.5, len = 31)
#qform <- function(x, y) 3*x^2 + y^2 - 2*x*y
#z <- outer(x, x, FUN = qform)

#par("mar" = c(5, 4, 4, 10) + 0.1) # wide righthand margin
#image(x, x, z, col=gray(6:12/15))
#image.scale(z, gray(6:12/15)) # the default

#image(x, x, z, col=gray(6:12/15))
#image.scale(z, gray(6:12/15), labels="range") # with range labels

# play around with the following ...
#image(x, x, z, col=gray(6:12/15))
#image.scale(z, gray(6:12/15), x=locator(1)) # or locator(2)
#}