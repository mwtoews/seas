\name{plot.interarrival}
\alias{plot.interarrival}
\title{Plot interarrivals for precipitation}
\description{
  Plots interarrivals for precipitation using boxplots, giving the
  typical number of continuous wet days and dry days (or spells)
  throughout the season. The mean value is also drawn as a single line.
}
\usage{
\method{plot}{interarrival}(x, width = 11, start = 1, rep = 0, start.day = 1,
           ylog = FALSE, maxy, main, \ldots)
}
\arguments{
  \item{x}{an \code{\link{interarrival}} object with numbers of
    dry/wet days}
  \item{width}{size of bin; see \code{\link{mkseas}}}
  \item{start}{starting bin number; e.g., if \code{width="mon"} and
    \code{start=5}, the plot will start on "May" at the left-hand side}
  \item{rep}{repetition of the bins in the boxplot}
  \item{start.day}{when \code{width} is numeric, this is the starting
    day of the year for the fist bin, or it can be a \code{Date} to
    specify a month and day (year is ignored)}
  \item{ylog}{\code{logical}; y-axis is logarithmic}
  \item{maxy}{maximum number of days for the y-axis; it can either be
    passed as \code{c(wet, dry)}, or as a single value for both}
  \item{main}{main title for plot, otherwise other title will be
    automatically generated}
  \item{\ldots}{ignored}
}
\author{Mike Toews}
\seealso{\code{\link{interarrival}}, \code{\link{seas.var.plot}}}
\examples{
data(mscdata)

dat.int <- interarrival(mksub(mscdata, id=1108447))
plot(dat.int, width="mon")

plot(dat.int, ylog=FALSE, maxy=35, rep=10)
}
\keyword{hplot}
