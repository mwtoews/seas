\name{plot.seas.norm}
\alias{plot.seas.norm}
\alias{plot.precip.norm}
\title{Plot seasonal normal of a variable, including precipitation normals}
\description{
  Plots a \dQuote{normal} of a seasonal variable, including a
  precipitation normal (which shows rain and snow fractions, where
  available). Significant missing data values are also indicated.
}
\usage{
\method{plot}{seas.norm}(x, start = 1, rep = 0, ylim,
     varwidth = FALSE, normwidth = FALSE,
     leg, add.alt = FALSE, main, ylab, \ldots)
}
\arguments{
  \item{x}{a \code{seas.norm} object created by either
    \code{\link{seas.norm}} or \code{\link{precip.norm}}}
  \item{start}{starting bin}
  \item{rep}{repeat bins}
  \item{ylim}{range of  y-axis; either as a single value,
    \code{c(0, max)}, or as two values \code{c(min, max)}}
  \item{varwidth}{\code{logical}; varies the width of each bar directly
    proportional to the frequency of active days (defined by a
    threshold); the value is normalized according to the next argument}
  \item{normwidth}{normalizes the width of the bars to a fixed
    \code{numeric} value (in days), or the maximum value if given
    \code{TRUE}; the default \code{FALSE} value normalizes each bar to
    the number of potentially active days}
  \item{leg}{if \code{TRUE} shows a legend summary of the statistics in
    the upper left hand corner; it can also be a \code{c(x, y)} pair or
    \dQuote{\code{\link{locator}}} to manually place the legend on the
    active graphics device}
  \item{add.alt}{\code{logical}; adds imperial units on the right-hand
    y-axis}
  \item{main}{title for plot; if it is missing, then it will
    automatically be generated}
  \item{ylab}{y-axis label; if it is missing, then it will automatically
    be generated}
  \item{\ldots}{ignored}
}
\details{
  The \code{varwidth} variable is useful for separating different
  precipitation patterns throughout the season.  It changes the width of
  the bar proportional to the frequency of precipitation events within
  the bin. Ideally, the bars will be tall and narrow with intense storms
  that occur infrequently, such as convective storms. Conversely the
  bars will be broader with less-intense rainfall events occurring more
  frequently.
}
\author{Mike Toews}
\seealso{\code{\link{seas.norm}}, \code{\link{precip.norm}},
  \code{\link{seas.sum}}}
\examples{
data(mscdata)
dat <- mksub(mscdata, id=1108447)
d.ss <- seas.sum(dat)
plot(seas.norm(d.ss))
plot(precip.norm(d.ss, fun=median))
plot(precip.norm(d.ss, fun=mean))
plot(precip.norm(d.ss, fun=mean, norm="active"))
plot(precip.norm(d.ss, fun=median, norm="active"))
plot(precip.norm(d.ss), start=15, rep=12)
mar <- par("mar")
plot(precip.norm(d.ss), add.alt=TRUE)

par(mar=mar)
d2.ss <- seas.sum(dat, start.day=as.Date("2000-08-01"))
plot(precip.norm(d2.ss, fun="mean"))
}
\keyword{hplot}
