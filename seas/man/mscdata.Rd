\name{mscdata}
\alias{mscdata}
\docType{data}
\encoding{latin1}
\title{Meteorological Service of Canada sample climate data}
\description{
  Sample climate data from the Meteorological Service of Canada
  (\acronym{MSC}) climate stations in western Canada.
}
\usage{data(mscdata)}
\format{
  A \code{\link{data.frame}} with 26358 daily observations on the
  following 10 variables (metric units of \sQuote{\enc{°}{degrees }C}
  and \sQuote{mm} \emph{per day}):
  \describe{
    \item{\code{id}:}{\code{\link{factor}} used to distinguish multiple
      stations within a single data frame}
    \item{\code{year}:}{\code{\link{integer}} year}
    \item{\code{yday}:}{\code{\link{integer}} day of year; 1--365 or
      1--366}
    \item{\code{date}:}{\code{\link{Date}} class}
    \item{\code{t_max}:}{daily maximum temperature}
    \item{\code{t_min}:}{daily minimum temperature}
    \item{\code{t_mean}:}{daily mean temperature}
    \item{\code{precip}:}{total daily precipitation}
    \item{\code{rain}:}{total daily liquid-phase precipitation}
    \item{\code{snow}:}{total daily solid-phase precipitation}
  }
  The climate variables have attributes (\code{\link{attr}} of
  \code{units} and \code{long.name} to identify their units and long
  names for plotting labels.

  There are three climate stations in this data frame from:
  \tabular{lll}{
    \bold{ID} \tab \bold{Station Location} \tab \bold{Province}\cr
    1096450   \tab Prince George           \tab \acronym{BC}\cr
    1108447   \tab Vancouver               \tab \acronym{BC}\cr
    2100630   \tab Haines Junction         \tab \acronym{YT}
  }

  All data spans from 1975 to 2004 for each station. Missing values are
  present.
}
\details{
  The field \code{id} is optional, but very handy when handling multiple
  stations. Also, the day of year (\code{yday}) and \code{year} are
  optional, since these are stored in the \code{date}, using \cr
  \code{dat$date <- as.Date(paste(dat$year,dat$yday),"\%Y \%j")}.

  The \code{units} and \code{long.name} attributes stored in the climate
  variables are optional, but help annotate the graphics.
}
\source{
  Data provided by the Meteorological Service of Canada, with
  permission. This data may only be reproduced for personal use; any
  other reproduction is permitted only with the written consent of
  Environment Canada.\cr

  \url{http://www.msc.ec.gc.ca/}
  \cr\url{http://climate.weatheroffice.gc.ca/contacts/}
}
\seealso{
  \code{\link{mscstn}} has MSC station ID codes, locations and names;
  \code{\link{mksub}} produces subsets of data;  \code{\link{read.msc}}
  reads MSC archive files, such as \code{\link{A1128551.DLY}}
}
\author{Mike Toews}
\examples{
data(mscstn)
data(mscdata)
par.orig <- par(no.readonly=TRUE)

# structure in R
str(mscdata)

# first few rows
head(mscdata)

# here are all the station IDs
stnids <- levels(mscdata$id)

# show all data
rng.p <- range(mscdata$precip, na.rm=TRUE)
rng.t <- range(mscdata$t_mean, na.rm=TRUE)
par(mfcol=c(2, 3), mgp=c(2, 1, 0), mar=c(3, 3, 3, 1), bty="l")
for (n in levels(mscdata$id)) {
  dat <- mscdata[mscdata$id == n,]
  plot(t_mean ~ date, dat, "l", col="red", ylim=rng.t)
  abline(h=0)
  plot(precip ~ date, dat, "l", col="blue", ylim=rng.p, main=n)
}
par(par.orig)

# show stations and station names available in this data frame
data.frame(stnids, name=getstnname(stnids))
dat <- mksub(mscdata, id=1108447)
dat$month <- mkseas(dat, "mon")
plot(t_mean ~ date, dat, "l")
plot(t_mean ~ date, dat, subset=(month == "Dec"))
seas.temp.plot(dat)
year.plot(dat)

# plot high-resolution statistics
dly.tmp <- tapply(dat$t_mean, dat$yday,
  quantile, c(5, 25, 50, 75, 95) / 100, na.rm=TRUE)
dly <- data.frame(yday=1:366,
  t(matrix(unlist(dly.tmp), nrow=5)))
names(dly) <- c("yday", "d5", "d25", "median", "d75", "d95")
plot(median ~ yday, dly, "n", ylim=c(-5, 25),
  ylab="mean temperature", xlab="day of year")
polygon(c(1:366, 366:1), c(dly$d5, rev(dly$d95)),
        border=FALSE, col="grey80")
polygon(c(1:366, 366:1), c(dly$d25, rev(dly$d75)),
        border=FALSE, col="grey50")
lines(median ~ yday, dly)
abline(h=0)
}
\keyword{datasets}
