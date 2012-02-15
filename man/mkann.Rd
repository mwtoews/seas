\name{mkann}
\alias{mkann}
\title{Make annum from a date}
\description{
  Discretizes a date into an annum, using a starting day to specify the
  start of a season, and ends in the next year.
}
\synopsis{
mkann(x, start.day, calendar)
}
\usage{
# normal usage, where 'x' is a data.frame with a date column
mkann(x)

# all options
mkann(x, start.day, calendar)

}
\arguments{
  \item{x}{A \code{data.frame} with a \code{date} column
    (of \code{\link{Date}} or \code{\link{POSIXct}} class)

    It may also be a vector of \code{Date} or
    \code{POSIXct} class}
  \item{start.day}{This is the starting day of the annum, and can be
    specified as either a \code{Date}, where year is ignored (e.g.,
    \code{as.Date("2000-08-01")} for August 1st of any year); or it can
    be a day of the year, from 1--365}
  \item{calendar}{if unspecified, it will be attempted to be read from
    \code{attr(x$date}); otherwise it is assumed to be a normal
    proleptic Gregorian calendar; see \code{\link{year.length}}}
}
\details{
  This date function finds the annual-breaks between seasons, using a
  \code{start.day}. Often, the \code{start.day} is 1, or January 1st,
  in which case simply the year is returned, since the season starts on
  January 1 and ends on December 31st. Otherwise, each annual break is
  set using \code{start.day}, and the annum is identified by the range
  of years, for example \code{1991_1992}, identifying a season starting on
  \code{start.day} in 1991, and ending in the day before
  \code{start.day} in 1992.

  The length of each year depends on the calendar; see
  \code{\link{year.length}} for details.

  A choice of \code{start.day} can influence annual totals using
  \code{\link{seas.sum}}, such as annual precipitation. For instance, if
  a particular winter in the Northern hemisphere has snow before and
  after the new year, these would be divided counting annual sums based
  on the year, whereas if \code{start.day} were before the winter
  season, the annual sum would be calculated throughout the winter
  season.
}
\value{
  Returns \code{\link{factor}}s for each date given in \code{x}, grouped
  by each annum.
}
\author{M.W. Toews}
\references{\url{http://en.wikipedia.org/wiki/Gregorian_calendar}}
\seealso{\code{\link{mkseas}}, \code{\link{seas.sum}}}
\examples{
data(mscdata)
dat <- mksub(mscdata,id=1108447)
dat$ann1 <- mkann(dat,start.day=1)
dat$ann2 <- mkann(dat,start.day=as.Date("2000-02-01"))
dat$ann3 <- mkann(dat,start.day=as.Date("2000-08-01"))
table(dat$ann1)
table(dat$ann2)
table(dat$ann3)
dat[26:36,c("date",paste("ann",1:3,sep=""))]
}
\keyword{utilities}
\keyword{datagen}
\keyword{ts}
