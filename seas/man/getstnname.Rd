\name{getstnname}
\alias{getstnname}
\title{Get station name}
\description{
  Retrieves the full name from \code{\link{mscstn}} using an ID
}
\usage{
getstnname(id)
}
\arguments{
  \item{id}{\code{\link{numeric}} or \code{\link{character}} ID array}
}
\details{
  This function simply converts the ID used in climate data frames into
  a meaningful name using \code{mscstn}. Presently it is useful only for
  Meteorological Service of Canada weather stations in BC, AB and YT,
  however \code{getstnname} can be overridden by another (similar)
  function and data object for other regions.
}
\value{
  Returns the station name(s). If the ID does not exist, returns \code{NULL}.
}
\author{Mike Toews}
\seealso{\code{\link{mscstn}}, \code{\link{mscdata}}, \code{\link{.seastitle}}}
\examples{
data(mscdata)

mscdata$id[1]
getstnname(mscdata$id[1])

ids <- levels(mscdata$id)
data.frame(id=I(ids), name=getstnname(ids))
}
\keyword{utilities}
