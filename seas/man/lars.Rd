\name{lars}
\alias{lars}
\alias{write.lars}
\alias{read.lars}
\alias{lars2help}
\alias{Summerland.st}
\alias{Summerland.dat}
\alias{write.lars.scenario}
\title{Read and write data from LARS-WG file formats}
\description{
  Read and write data from the LARS-WG stochastic weather generator
  file formats; also convert to a format for HELP
}
\usage{
# read synthetic or observed *.st file
read.lars(stfile, year.offset = 0)

# write observed climate data (*.st and/or *.sr)
write.lars(x, stfile, datfile, site, lat, lon, alt)

# experimental functions (may not work great; or at all!)
lars2help(infile, outfile, year.offset, site)
write.lars.scenario(file, x1, x2, name = "anomaly")
}
\arguments{
  \item{stfile}{file name with \file{*.st} extension; this is a
    \sQuote{site file} for LARS-WG which contains meta-data for the
    climate data, and has the location of the the climate data file; for
    \code{write.lars}, if this variable is \code{\link{NA}} or
    \code{\link{FALSE}}, this file will not be written (however,
    \code{datfile} must be defined)}
  \item{datfile}{file name with either \file{*.sr} or \file{*.dat}
    extension; contains climate data, as described by \code{stfile};
    this does not need to be set if \code{stfile} is defined, as this
    datum is found in the \file{st} file}
  \item{file}{file name with a \file{*.sce} extension; this is a
    \sQuote{scenario} file with absolute and relative changes of climate
    data}
  \item{infile}{input file}
  \item{outfile}{output file}
  \item{x}{\code{data.frame} of climate data}
  \item{x1}{same as \code{x}}
  \item{x2}{same as \code{x}}
  \item{year.offset}{offset of years between what is contained in the
    data files and what is needed in \R to produce a reasonable
    \sQuote{\code{\link{Date}}}; this is required, for example, if
    synthetic data are produced that start from an arbitrary year
    \sQuote{1} but represent climate from the year \sQuote{2000}}
  \item{site}{same as \sQuote{[SITE]} in \file{st} file; if missing,
    this will try to read from \code{attr(x$name)}; this is the same as
    a \sQuote{region} for HELP}
  \item{name}{scenario name}
  \item{lat}{same as \sQuote{LAT} in \file{st} file; if missing, this
    will try to be read from \code{attr(x$latitude)}}
  \item{lon}{same as \sQuote{LON} in \file{st} file; if missing, this
    will try to be read from \code{attr(x$longitude)}}
  \item{alt}{same as \sQuote{ALT} in \file{st} file; if missing, this
    will try to be read from \code{attr(x$elevation)}}
}
\details{
  These functions interface with the LARS-WG files (Version 4.0), which
  is a stochastic weather generator by Mikhail Semenov.

  The climate data files used with LARS-WG have two parts: (1)~a
  \sQuote{site file} with a \file{st} extension, containing the
  meta-data; and (2)~a data file with a \file{*.sr} or \file{*.dat}
  extension, containing all the data. The variable names are translated
  according to the following table:

  \tabular{ll}{
    \emph{seas}  \tab \emph{LARS-WG} \cr
    \code{year}  \tab \sQuote{YEAR} \cr
    \code{yday}  \tab \sQuote{JDAY} \cr
    \code{t_min} \tab \sQuote{MIN} \cr
    \code{t_max} \tab \sQuote{MAX} \cr
    \code{preicp}\tab \sQuote{RAIN} \cr
    \code{solar} \tab \sQuote{RAD} \cr
    \code{sun}   \tab \sQuote{SUN} \cr
    \code{pet}   \tab \sQuote{PET} \cr
  }

  To write climate data from \R to a LARS-WG file, the \code{data.frame}
  names need to match those in the \pkg{seas}-side of the table.

  Data exported from \code{write.lars} always has legal (according to
  the Gregorian calendar) and increasing sequence of days (even if there
  are gaps in \code{x$date}). Missing data values are written as
  \code{-99}.

  Synthetically generated data from LARS-WG use a 365-day calendar, and
  may need to be converted to a Gregorian calendar, which can be done
  using \code{\link{conv365toGregorian}}.

  \code{lars2help} and \code{write.lars.scenario} are experimental
  functions to translate data between LARS and HELP (see
  \code{\link{write.help}} for more info).
}
\author{Mike Toews}
\references{
  LARS-WG was can be downloaded for academic and research uses from \cr
  \url{http://resources.rothamsted.ac.uk/mas-models/larswg}

  Semenov, M.A. and Barrow, E.M. 1997. Use of a stochastic
    weather generator in the development of climate change
    scenarios. \emph{Climate Change}, \bold{35} (4), 397--414,
    \doi{10.1023/A:1005342632279}
}
\seealso{\code{\link{write.help}}, \code{\link{read.sdsm}},
  \code{\link{summerland}} example synthetic data,
  \code{\link{conv365toGregorian}}}
\examples{
stfile <- system.file("extdata", "summerland.st", package="seas")
print(stfile)
summ <- read.lars(stfile, year.offset=1960)
head(summ)
str(summ)

# plot temperature
summ$t_mean <- rowMeans(summ[, c("t_min", "t_max")])
seas.temp.plot(summ)

# plot solar radiation
seas.var.plot(summ, "solar")

# plot precipitation
summ.ss <- seas.sum(summ)
image(summ.ss)
plot(seas.norm(summ.ss))
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
