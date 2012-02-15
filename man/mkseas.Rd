\name{mkseas}
\alias{mkseas}
\title{Make a date into a seasonal factor}
\description{
  Discretizes a date within a year into a bin (or \code{\link{factor}})
  for analysis, such as 11-day groups or by month.
}
\usage{
mkseas(x, width = 11, start.day = 1, calendar, year)
}
\arguments{
  \item{x}{A \code{data.frame} with a \code{date} column
    (of \code{\link{Date}} or \code{\link{POSIXct}} class)
    
    It can also be an integer specifying the Julian day (specify
    \code{year} to determine the leap year)
    
    If it is omitted, the full number of days will be calculated for the
    year, determined by either \code{year} or \code{calendar}}
  \item{width}{either \code{numeric} or other \code{character} value; if
    it is numeric, it specifies the number of days in each bin (default
    is 11 days); if character it specifies a common calendar usage, such as
    \code{"mon"} for months; see details below}
  \item{start.day}{this is the start of the season, specified as either a
    as a \code{Date} to specify a month and day (year is ignored; day of
    month is ignored if \code{width} relates to a month), or as a
    \code{\link{numeric}} day of year, between 1 and the number of days
    for the calendarter a leap day)}
  \item{calendar}{used to determine the number of days per year and per
    bin; if not specified, a proleptic Gregorian calendar is assumed;
    see \code{\link{year.length}}}
  \item{year}{required if \code{x} is omitted, or if \code{x} is a
    Julian day integer and \code{width} is non-numeric; used to
    calculate leap year}
}
\details{
  This useful date function groups \emph{days} of a year into discrete
  bins (or into a \code{\link{factor}}).  Statistical and plotting
  functions can be applied to a variable contained within each bin. An
  example of this would be to find the monthly temperature averages,
  where \emph{month} is the bin.
  
  If \code{width} is \code{\link{integer}}, the width of each bin
  (except for the last) will be exactly \code{width} days. Since the
  number of days in a year are not consistent, nor are always perfectly
  divisible by \code{width}, the numbers of days in the last bin will
  vary. \code{mkseas} determines that last bin must have at least 20\% of
  the number of observations for a leap year, otherwise it is merged
  into the second to last bin (which will have extra numbers of
  days). If \code{width} is \code{\link{numeric}} (i.e. \code{366/12}),
  the width of each bin varies slightly. Using \code{width = 366/12} is
  slightly different than \code{width = "mon"}. Leap years only affect
  the \emph{last} bin.
  
  Other common classifications based on the Gregorian calendar can be
  used if \code{width} is given a \code{\link{character}} array. All of
  these systems are arbitrary: having different numbers of days in each
  bin, and leap years affecting the number of days in February. The most
  common, of course, is by \emph{month} (\code{"mon"}). Meteorological
  quarterly seasons (\code{"DJF"}) are based on grouping three months,
  starting with December.  This style of grouping is commonly used in
  climate literature, and is preferred over the season names
  \sQuote{winter}, \sQuote{spring}, \sQuote{summer}, and
  \sQuote{autumn}, which apply to only one hemisphere.  The less common
  annual quarterly divisions (\code{"JFM"}) are similar, except that
  grouping begins with January. Zodiac divisions (\code{"zod"}) are
  included for demonstrative purposes, and are based on the Tropical
  birth dates (common in Western-culture horoscopes) starting with Aries
  (March 21).

  Here are the complete list of options for the \code{width} argument:
  \itemize{
    \item \code{numeric}: the width of each bin (or group) in days
    \item \code{366/n}: divide the year into \code{n} sections
    \item \code{"mon"}: month intervals (abbreviated month names)
    \item \code{"month"}: month intervals (full month names)
    \item \code{"DJF"}: meteorological quarterly divisions: DJF, MAM, JJA, SON
    \item \code{"JFM"}: annual quarterly divisions: JFM, AMJ, JAS, OND
    \item \code{"JF"}: annual six divisions: JF, MA, AJ, JA, SO, ND
    \item \code{"zod"}: zodiac intervals (abbreviated symbol names)
    \item \code{"zodiac"}: zodiac intervals (full zodiac names)
    %\item \code{"zod.s"}: zodiac intervals (symbols; requires \R Unicode support)
  }

  If a non-Gregorian calendar is used (see \code{\link{year.length}}),
  the number of days in a year can be set using \code{calendar}
  attribute in the \code{date} column (using \code{\link{attr}}).
  For example, \code{attr(x$date,"calendar") <- "365_day"} will set the
  dates using a 365-day per year calendar, where February is always
  28-days in length. If this attribute is not set, it is assumed a
  normal Gregorian calendar is used. Calendars with 360-days per year
  (or 30-days per month) are incorrectly handled, since February cannot
  have 30 days, however this can be forced by including a duplicate
  February date in \code{x} for each year).
}
\value{
  Returns an array of \code{\link{factor}}s for each date given in \code{x}.
  The factor also has four attributes: \code{width}, \code{start.day},
  \code{calendar} (assumed to be 366, unless from attribute set in
  \code{\link{Date}}), and an array \code{days} showing the maximum
  number of days in each bin.
  
  See examples for its application.
}
\note{
  The phase of the Gregorian solar year (begins Julian day 1, or January
  1st) is not in sync with the phase of \code{"DJF"} (begins Julian day
  335/336) or \code{"zod"} (begins Julian day 80/81). If either of these
  systems are to be used, ensure that there are \emph{several} years of
  data, or that the phase of the data is the same as the beginning
  Julian day.
  
  For instance, if one years worth of data beginning on Julian day 1 is
  factored into \code{"DJF"} bins, the first bin will mix data from the
  first three months, and from the last month. The last three bins will
  have a continuous set of data. If the values are not perfectly
  periodic, the first bin will have higher variance, due to the mixing
  of data separated by nearly a year.
}
\section{Locale warning}{
  Month names generated using \code{"mon"} or \code{"months"} are locale
  specific, and depend on your operating system and system language
  settings. Normally, abbreviated month names should have exactly three
  characters or less, with no trailing decimals. However,
  Microsoft-based operating systems have an inconsistent set of
  abbreviated month names between locales. For example, abbreviated
  month names in English locales have three letters with no period at
  the end, while French locales have 3--4 letters with a decimal at the
  end. If your OS is POSIX, you should have consistent month names in
  any locale. This can be fixed by setting
  \code{options("seas.month.len") <- 3}, which forces the length of the
  months to be three-characters in length.
  
  To avoid any issues supporting locales, or to use English month names,
  simply revert to a C locale: \code{\link{Sys.setlocale}(loc="C")}.
}
\author{M.W. Toews}
\references{\url{http://en.wikipedia.org/wiki/Solar_calendar}}
\seealso{\code{\link{mkann}}, \code{\link{seas.sum}}}
\examples{
# Demonstrate the number of days in each category
ylab <- "Number of days"

barplot(table(mkseas(width="mon", year=2005)),
  main="Number of days in each month",ylab=ylab)

barplot(table(mkseas(width="zod", year=2005)),
  main="Number of days in each zodiac sign",
  ylab=ylab)

barplot(table(mkseas(width="DJF", year=2005)),
  main="Number of days in each meteorological season",
  ylab=ylab)

barplot(table(mkseas(width=5, year=2004)),
  main="5-day categories", ylab=ylab)

barplot(table(mkseas(width=11, year=2005)),
  main="11-day categories",ylab=ylab)

barplot(table(mkseas(width=366/12, year=2005)),
  main="Number of days in 12-section year",
  sub="Note: not exactly the same as months")

# Application using synthetic data
dat <- data.frame(date=as.Date(paste(2005,1:365),"\%Y \%j"),
  value=(-cos(1:365*2*pi/365)*10+rnorm(365)*3+10))
attr(dat$date,"calendar") <- "365_day"

dat$d5 <- mkseas(dat,5)
dat$d11 <- mkseas(dat,11)
dat$month <- mkseas(dat,"mon")
dat$DJF <- mkseas(dat,"DJF")

plot(value ~ date, dat)
plot(value ~ d5, dat)
plot(value ~ d11, dat)
plot(value ~ month, dat)
plot(value ~ DJF, dat)

head(dat)

tapply(dat$value, dat$month, mean, na.rm=TRUE)
tapply(dat$value, dat$DJF, mean, na.rm=TRUE)

dat[which.max(dat$value),]
dat[which.min(dat$value),]

# start on a different day
st.day <- as.Date("2000-06-01")

dat$month <- mkseas(dat,"mon",start.day=st.day)
dat$d11 <- mkseas(dat,11,start.day=st.day)
dat$DJF <- mkseas(dat,"DJF",start.day=st.day)

plot(value ~ d11, dat,
     main=.seasxlab(11,start.day=st.day))
plot(value ~ month, dat,
     main=.seasxlab("mon",start.day=st.day))
plot(value ~ DJF, dat,
     main=.seasxlab("DJF",start.day=st.day))

}
\keyword{utilities}
\keyword{datagen}
\keyword{ts}
