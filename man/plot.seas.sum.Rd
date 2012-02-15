\name{plot.seas.sum}
\alias{plot.seas.sum}
\alias{plot.precip.sum}
\title{Plot boxplots of normalized seasonal sums}
\description{
  Plots normalized seasonal sums using boxplots.
}
\usage{
\method{plot}{seas.sum}(x, var, norm="days", year.filter, ylim,
    start=1, rep=0, col = "lightgrey", main, ylab, \ldots)
}
\arguments{
  \item{x}{a \code{seas.sum} object created by \code{\link{seas.sum}}}
  \item{var}{name of seasonal variable in \code{x}}
  \item{norm}{a variable to normalize by, either \code{"days"} (to
    produce \emph{unit/day}) or \code{"active"} (\emph{unit/day}, when
    active); it may also be a \code{matrix} with the same dimensions as
    \code{x$days}}
  \item{year.filter}{use only these years for analysis}
  \item{ylim}{either a single value for \code{c(0,ylim)}, or a range of
    \code{c(min,max)} for the y-axis limits}
  \item{start}{starting bin at left-hand side of plot}
  \item{rep}{repeat bins on right-hand side of plot}
  \item{col}{colour for boxplot, default is \code{"lightgrey"}}
  \item{main}{title for plot; if it is missing, then it will
    automatically be generated}
  \item{ylab}{y-axis label; if it is missing, then it will automatically
    be generated}
  \item{\ldots}{ignored}
}
\details{
  This function is a boxplot interpretation of a \code{seas.sum}
  object. This is not the same as treating \code{var} as a continuous
  variable and using \code{plot.seas.var}, since a \code{seas.sum}
  object has been smoothed. Daily extreme values are not well
  represented here as a result.
}
\section{Warning}{
  The appearance of the boxplots are sensitive to the \code{width}
  parameter specified in the \code{seas.sum} function on strongly
  discontinuous variables. Small bin widths capture the discontinuities
  better than wider bins, and changes the skew of the distribution.

  For instance, the median will appear to decrease as \code{width} decreases.
}
\author{M.W. Toews}
\seealso{\code{\link{seas.sum}}, \code{\link{image.seas.sum}}, \code{\link{seas.norm}}}
\examples{
data(mscdata)
par.orig <- par(no.readonly=TRUE)
on.exit(par.orig)

dat <- mksub(mscdata, id=1108447)
dat.ss <- seas.sum(dat)

# Normalized by the number of days in each bin
plot(dat.ss)

# Normalized by the number of active days in each bin
plot(dat.ss,norm="active")

# Snow, using a different start day, and a better y-axis:
dat2.ss <- seas.sum(dat,var="snow",width="mon",
                    start.day=as.Date("2000-08-01"))
par(yaxs="i")
plot(dat2.ss,var="snow")
plot(dat2.ss,var="snow",norm="active")

}
\keyword{hplot}
