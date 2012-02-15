\name{year.length}
\alias{year.length}
\title{Calculate the number of days in a year}
\description{
  Determines the number of days per year using a given calendar.
}
\usage{
year.length(x, calendar)
}
\arguments{
  \item{x}{year or a \code{\link{Date}}}
  \item{calendar}{calendar, see details}
}
\details{
  The number of days per year depends on the choice of
  calendar. Calendar names used in the function are the same defined
  for the CF conventions, used for netCDF files. If a calendar is not
  specified (or \code{NULL}), then it is assumed to
  be a proleptic Gregorian calendar (which extends before
  1582-10-15). Other accepted calendars are:
  \itemize{
    \item \code{"360"}: always 360-days per year
    \item \code{"365_day"} or \code{"noleap"}: always 365-days per year
    \item \code{"366"} or \code{"all_leap"}: always 366-days per year
    \item \code{"julian"}: 366 days on years divisible by 4, otherwise
    365 days
  }
  
}
\value{
  Returns a vector the same length as \code{x} with the numbers of days
  corresponding to each year.
}
\author{M.W. Toews}
\references{\url{http://www.cgd.ucar.edu/cmw/eaton/cf-metadata/CF-current.html#cal}}
\seealso{\code{\link{mkseas}}, \code{\link{mkann}}}
\examples{
cal <- data.frame(year=c(1899,1900,1904,2000,2080,2100))
cal[["Gregorian"]] <- year.length(cal$year)
cal[["Julian"]] <- year.length(cal$year,"julian")
cal[["360_day"]] <- year.length(cal$year,"360_day")
cal[["365_day"]] <- year.length(cal$year,"365_day")
cal[["366_day"]] <- year.length(cal$year,"366_day")

cal
}
\keyword{utilities}
\keyword{datagen}
\keyword{ts}
