\name{precip.dep}
\alias{precip.dep}
\title{Cumulative precipitation departure}
\description{
  Calculate the cumulative precipitation departure (\acronym{CPD}) for a
  station with a given precipitation normal.
}
\usage{
precip.dep(x, norm, var = "precip")
}
\arguments{
  \item{x}{a seasonal \code{data.frame} of climate data}
  \item{norm}{a \code{precip.norm} object containing the precipitation
    normal for the same station as \code{x}}
  \item{var}{a common seasonal variable found in \code{x} and \code{norm}}
}
\details{
  This function is useful for looking at the behaviour of a precipitation
  time-series in relation to its precipitation normal over an extended
  period of time. This is especially useful for identifying changes in
  precipitation, and is useful for relating to groundwater recharge
  patterns
  (e.g., \url{http://www.env.gov.bc.ca/wsd/data_searches/obswell/obsw002.html}).
}
\value{
  Returns a \code{data.frame} similar to \code{x}, but contains the
  departures in the \code{dep} column.
}
\author{Mike Toews}
\note{
  The selection of \code{fun} in \code{\link{precip.norm}}, such as
  using \code{\link{mean}} or \code{\link{median}}, will affect
  the result of this function; \code{width} has only a minor effect.

  Periods with missing (\code{NA}) values in \code{var} of \code{x} will
  have a flat departure, neither increasing nor decreasing.
}
\seealso{\code{\link{precip.norm}}}
\examples{
data(mscstn)
data(mscdata)

dat <- mksub(mscdata, id=1108447)
dat.ss <- seas.sum(dat)
dat.dep <- precip.dep(dat,precip.norm(dat.ss, fun="mean"))
plot(dep ~ date, dat.dep, type="l", main="CPD from mean normals")

dat.dep <- precip.dep(dat, precip.norm(dat.ss, fun="median"))
plot(dep ~ date, dat.dep, type="l", main="CPD from median normals")
}
\keyword{manip}
\keyword{datagen}
