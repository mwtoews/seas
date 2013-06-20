\name{year.ploy}
\alias{year.plot}
\alias{plot.year}
\title{Plot annual temperature and precipitation statistics}
\description{
  Plots a continuous set of annual temperature and precipitation
  statistics for a single climate station.
}
\usage{
year.plot(x, start.day = 1, precip.only = FALSE, precip.ylim,
          temp.ylim, na.cut = 10, ...)
}
\arguments{
  \item{x}{a \code{data.frame} of climate data}
  \item{start.day}{starting day of annum; either a \code{Date} or an
    integer day of year; this influences the statistics for each year or
    annum; such as annual precipitation sums}
  \item{precip.only}{only precipitation data is used; rain and snow ignored}
  \item{precip.ylim}{range for precipitation graph}
  \item{temp.ylim}{range for temperature graph}
  \item{na.cut}{minimum number of missing data points in a year to make
    it void; temperature and precipitation are treated independently}
  \item{...}{ignored}
}
\details{
  This simply shows temperature using (\code{\link{boxplot}}s) and
  annual precipitation totals. The red bars are directly proportional to
  the fraction of missing (or \code{NA}) values for the year; statistics
  not shown if there are more than \code{na.cut} \code{NA} values in a
  given year.
}
\note{
  This function was formerly named \code{plot.year}, but required
  renaming as it is not an S3 method.
}
\author{Mike Toews}
\seealso{
  \code{\link{mscdata}}, \code{\link{seas.temp.plot}},
  \code{\link{plot.seas.norm}} (can be used for precipitation normals),
  calculate statistics with \code{\link{tapply}}}
\examples{
data(mscdata)

year.plot(mksub(mscdata, id=1108447))

year.plot(mksub(mscdata, id=1108447,
                start=as.Date("1975-08-01"),
                end=as.Date("2004-07-31")),
          start.day=as.Date("2000-08-01"))
}
\keyword{hplot}
