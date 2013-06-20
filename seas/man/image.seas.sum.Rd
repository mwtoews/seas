\name{image.seas.sum}
\alias{image.seas.sum}
\title{Show a seasonal sum data object}
\description{
  Graphically display a seasonal sum object, as well as the method of
  solution of the median/quantile \dQuote{normal}
}
\usage{
\method{image}{seas.sum}(x, var, norm = "days", start = 1, rep = 0, zlim, alim,
    palette = colorRampPalette(c("white", "blue"))(64),
    year.filter, power, contour = TRUE, show.median, main, \ldots)
}
\arguments{
  \item{x}{a \code{\link{seas.sum}} object}
  \item{var}{the desired variable to show, otherwise will use the prime
    variable, defined in \code{x}}
  \item{norm}{variable to normalize by, usually \code{"days"}, to
    produce \emph{unit/day}}
  \item{start}{starting bin number; e.g., for monthly sums, if
    \code{start=5}, the plot will start on "May" at the left-hand side;
    \code{show.median} cannot be produced if \code{start} is greater
    than one, since the annual sums (row-wise) would be a mix of
    different annums}
  \item{rep}{repetition of the bins (columns)}
  \item{zlim}{range of normalized values displayed; this can be either a
    single number for the maximum (minimum set to zero), or a
    \code{c(min, max)} range with a defined minimum}
  \item{alim}{if \code{show.median}, this is the range for the annual
    sums; this can either be a singe number for the maximum (minimum set
    to zero, or a \code{c(min, max)} range with a defined minimum}
  \item{palette}{colours for \code{\link{image}}; the use of
    \code{\link{colorRampPalette}} is recommended}
  \item{year.filter}{specifies the annual seasons to display}
  \item{power}{this transforms the normalized values for the colours to
    a power (\code{\link{^}}), such as \code{0.5} for square-root
    (\code{\link{sqrt}}), or others; this can help improve the contrast
    in the display of data, but the quantities displayed in the
    colour-bar and contours remain untransformed}
  \item{contour}{\code{logical}; show contours in lower left-hand plot}
  \item{show.median}{\code{logical}; show how the median calculation is
    achieved graphically (computationally it is done using a secant
    method); see \code{\link{seas.norm}} for more information on this
    method; this can only be shown if the annums (rows) are complete, so
    \code{start} must be 1, and \code{rep} must be 0 (otherwise the
    row-wise sums would not be the annual sums)}
  \item{main}{main title for plot, otherwise it will automatically be
    generated; \code{NA} suppresses a title, and automatically adjusts
    the device margins}
  \item{\ldots}{ignored}
}
\details{
  This is a graphical representation of a \code{\link{seas.sum}} object,
  and is far more informative than a traditional precipitation
  \dQuote{normal} (i.e., \code{\link{precip.norm}} or
  \code{\link{precip.norm}})

  If \code{norm = "days"} and \code{show.median = TRUE} (default), the
  seasonal sums appear in right-hand frames. Horizontal and vertical
  lines indicate a \sQuote{normal} from the image, whereby the sum of
  the quantile is equal to the median of the annual amount. This
  numerical solution is found using \code{\link{seas.norm}}.
}
\author{Mike Toews}
\seealso{
  \code{\link{seas.sum}}, \code{\link{seas.norm}}

  See \code{\link{SeasOpts}} to modify other aspects of the plot}
\examples{
data(mscdata)
dat <- mksub(mscdata, id=1108447)

dat.ss <- seas.sum(dat, width="mon")
image(dat.ss)

image(dat.ss, contour=FALSE)

image(dat.ss, norm="active", start=6, rep=5)

# different start day (not Jan 1st)
dat2.ss <- seas.sum(dat, start.day=as.Date("2001-08-01"))
image(dat2.ss)
image(dat2.ss, power=2)
image(dat2.ss, palette=rainbow(64), main=NA)  # no title
image(dat2.ss, palette=colorRampPalette(c("white", "darkgreen"))(16))
image(dat2.ss, "snow")
image(dat2.ss, "snow", power=0.5)

# growing degree days for 10 degC
dat$gdd10 <- dat$t_mean - 10
dat$gdd10[dat$gdd10 < 0] <- 0
attr(dat$gdd10,"long.name") <- "growing degree days"
dat3.ss <- seas.sum(dat, var="gdd10")
image(dat3.ss, "gdd10", palette=colorRampPalette(c("white", "red"))(64))
}
\keyword{hplot}
