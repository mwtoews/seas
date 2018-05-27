\name{read.msc}
\alias{read.msc}
\encoding{UTF-8}
\title{Read a MSC archive file into a data.frame}
\description{
  Reads a Meteorological Service of Canada (\acronym{MSC}) digital
  archive files (HLY and DLY formats) into a data.frame.
}
\usage{
read.msc(file, flags = FALSE, add.elem, format, verbose = TRUE)
}
\arguments{
  \item{file}{file name (with path, if not in \code{\link{getwd}}); it
    can also be a \code{\link{connection}}, such as
    \code{\link{bzfile}}}
  \item{flags}{\code{\link{logical}} return the flags with the
    \code{\link{data.frame}}}
  \item{add.elem}{either a \code{\link{data.frame}} or a
    \code{\link{list}} with additional elements not found in this
    function}
  \item{format}{parameter ignored and will be removed in a future release}
  \item{verbose}{\code{\link{logical}} verbose output, such as number of
    stations, elements, records and years in the archive file}
}
\details{
  This function currently reads in HLY (hourly) and DLY (daily) archive
  formats. This is automatically detected. The other formats, FIF
  (fifteen-minute) and MLY (monthly), are not currently supported.

  The input file can include multiple stations and multiple elements
  (measured variables). The multiple stations are deciphered through
  the \code{id} column, and the multiple variables appear as columns to
  the output data frame.

  This function currently only reads a limited number of elements,
  however additional elements can be used by editing two lines in the R
  source for this function.
}
\section{Value}{
  Returns a \code{\link{data.frame}} object with the following minimum
  fields:

  \describe{
    \item{\code{id}:}{\code{\link{factor}} used to distinguish multiple
      stations within a single data frame}
    \item{\code{year}:}{\code{\link{integer}} year}
    \item{\code{yday}:}{\code{integer} day of year; 1--365 or 1--366}
    \item{\code{date}:}{\code{\link{Date}}, useful for plotting a continuous
      time-series}
    \item{\code{datetime}:}{\code{\link{POSIXct}}, includes date and time
      info, only included if \code{file} is in HLY archive format}
    \item{\code{element}:}{\code{\link{numeric}}, with
      \code{\link{attributes}} set for \code{units} and
      \code{long.name}; these can be changed using \code{\link{attr}} on
      \code{dat$varname}}
    \item{\code{flag}:}{\code{factor}; included if \code{flags=TRUE}}
  }

  The are as many \code{element} columns for each element found in the
  archive file, such as:
  \tabular{rlll}{
    \bold{alias} \tab \bold{name} \tab \bold{long.name} \tab \bold{units}\cr
    1   \tab \code{t_max}  \tab daily maximum temperature \tab
          \enc{°}{degrees }C \cr
    2   \tab \code{t_min}  \tab daily minimum temperature \tab
          \enc{°}{degrees }C \cr
    3   \tab \code{t_mean} \tab daily mean temperature \tab
          \enc{°}{degrees }C \cr
    10  \tab \code{rain}   \tab total rainfall \tab mm \cr
    11  \tab \code{snow}   \tab total snowfall \tab mm \cr
    12  \tab \code{precip} \tab total precipitation \tab mm \cr
    13  \tab \code{snow_d} \tab snow on the ground \tab cm \cr
    \ldots \tab \ldots \tab \emph{other elements} \tab \emph{optional}
  }

  Additional elements (or variables) can be added by specifying the
  \code{element} variable, and their units can be set using, for
  example, \code{attr(dat$var, "units") <- "cm"}.

  Units are in common metric units: \sQuote{mm} for precipitation-related
  measurements, \sQuote{cm} for snow depth, and
  \sQuote{\enc{?}{degrees }C} for temperature. The \code{flag} columns are
  a single character \code{\link{factor}}, described in the \acronym{MSC}
  Archive documentation. Units are added to each column using, for example
  \code{attr(dat$precip, "units") <- "mm"}.
}
\source{
  Climate data can be requested from \acronym{MSC}, or can be obtained
  directly from the Canadian Daily Climate Data (\acronym{CDCD})
  CD-ROMs, which are available for a free download (procedure described
  in \code{\link{A1128551.DLY}}).
}
\author{Mike Toews}
\references{
  \url{https://web.archive.org/web/20130625230337/http://climate.weatheroffice.gc.ca/prods_servs/documentation_index_e.html} (archived) Technical Documentation - Documentation for the Digital Archive of Canadian Climatological Data (Surface) Identified By Element

  \verb{http://climate.weatheroffice.gc.ca/prods_servs/index_e.html#cdcd} (dead link) CDCD CD-ROM download location
}
\seealso{\code{\link{mscstn}}, \code{\link{mksub}}, \code{\link{mkseas}},
    \code{\link{A1128551.DLY}}}
\examples{
fname <- system.file("extdata", "A1128551.DLY", package="seas")
print(fname)
dat <- read.msc(fname)
print(head(dat))

seas.temp.plot(dat)
year.plot(dat)

# Show how to convert from daily to monthly data
dat$yearmonth <- factor(paste(format(dat$date, "\%Y-\%m"), 15, sep="-"))
mlydat <- data.frame(date=as.Date(levels(dat$yearmonth)))
mlydat$year <- factor(format(mlydat$date, "\%Y"))
mlydat$month <- mkseas(mlydat, "mon")

# means for temperature data
mlydat$t_max <- as.numeric(
    tapply(dat$t_max, dat$yearmonth, mean, na.rm=TRUE))
mlydat$t_min <- as.numeric(
    tapply(dat$t_min, dat$yearmonth, mean, na.rm=TRUE))
mlydat$t_mean <- as.numeric(
    tapply(dat$t_mean, dat$yearmonth, mean, na.rm=TRUE))

# sums for precipitation-related data
mlydat$rain <- as.numeric(
    tapply(dat$rain, dat$yearmonth, sum, na.rm=TRUE))
mlydat$snow <- as.numeric(
    tapply(dat$snow, dat$yearmonth, sum, na.rm=TRUE))
mlydat$precip <- as.numeric(
    tapply(dat$precip, dat$yearmonth, sum, na.rm=TRUE))
print(head(mlydat), 12)

# Show how to convert from a HLY file into daily summaries
\dontrun{
hlydat <- read.msc(bzfile("HLY11_L1127800.bz2"), flags=TRUE)
hlydat$date <- factor(hlydat$date)

# sum the solar radiation for each day to find the 'total daily'
sumdat <- tapply(hlydat$solar, hlydat$date, sum, na.rm=TRUE)
dlydat <- data.frame(date=as.Date(names(sumdat)),
                     solar=as.numeric(sumdat))

# sum the number of hours without measurements
sumdat <- tapply(hlydat$solar, hlydat$date,
                 function(v)(24 - sum(!is.na(v))))
dlydat$na <- as.integer(sumdat)

# quality control to remove days with less than 4 hours missing
Summerland <- dlydat[dlydat$na < 4,]

attr(Summerland$solar, "units") <- "W/(m^2*day)"
attr(Summerland$solar, "long.name") <- "Daily total global solar radiation"
seas.var.plot(Summerland, var="solar", col="yellow", width=5)
}
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
