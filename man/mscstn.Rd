\name{mscstn}
\alias{mscstn}
\docType{data}
\title{Meteorological Service of Canada station information}
\description{
  Meteorological Service of Canada weather station data,
  including national ID, station ID, Province, latitude and longitude.
}
\usage{data(mscstn)}
\format{
  A \code{\link{data.frame}} with 4493 climate stations with the
  following 6 columns:
  \tabular{rl}{
    \code{name} \tab Full station name \cr
    \code{nid}  \tab National ID, alphanumeric key \cr
    \code{sid}  \tab Station ID, also used for airport codes \cr
    \code{prov} \tab Canadian Province \cr
    \code{lat}  \tab Decimal degrees latitude; NAD83 \cr
    \code{long} \tab Decimal degrees longitude; NAD83
  }
}
\details{
  This data object is used as a look-up table to convert a unique
  station identifier (\code{nid}) or ID into a station name, using
  \code{\link{getstnname}}.

  Currently, this data only includes weather stations from Alberta,
  British Columbia and the Yukon.
}
\source{
  Provided by the Meteorological Service of Canada
  (\url{http://www.msc.ec.gc.ca/}), with permission.
}
\seealso{\code{\link{getstnname}}, \code{\link{mscdata}},
  \code{\link{read.msc}}}
\author{Mike Toews}
\examples{
data(mscstn)
str(mscstn)

table(mscstn$prov)
plot(lat ~ long, mscstn, pch=".")
}
\keyword{datasets}
