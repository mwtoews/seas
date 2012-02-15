\name{A1128551.DLY}
\alias{A1128551.DLY}
\docType{data}
\title{MSC daily climate data file (DLY archive format) and instructions
  for Canadian Daily Climate Data CD-ROMs for analysis}
\description{
  Meteorological Service of Canada daily climate data (DLY archive
  format) from Vernon. This document also describes how to obtain data
  from the Canadian Daily Climate Data CD-ROMs for analysis in
  \pkg{seas}.
}
%\usage{data(mscdata)}
\format{
  MSC DLY archive format (4-digit year).
}
\details{
  The sample file name is \file{A1128551.DLY}, which contains daily
  climate data from Vernon, British Columbia. Load this file using
  \code{\link{read.msc}}.
  
  This file was created using the instructions below, with the addition
  of renaming the file extension from \file{*.ALL} to \file{*.DLY}.
}
\section{How to obtain Canadian Daily Climate Data}{
  Two CDCD CD-ROMs are currently available for free download, which have
  data from 11,216 locations throughout Canada.

  This procedure shows how to extract the data using \file{CDEX.EXE},
  which requires a DOS environment. There is, however, an alternative
  Python module, which can batch extract data from the CD-ROMs.  If you
  are using a non-Microsoft platform, you could try \sQuote{DOSBox} to
  emulate the DOS environment (tested on Debian and Mac OS X; hint:
  mount the CD-ROM drive by using \code{-t cdrom} option).

  To extract data from the CD-ROM:
  \enumerate{
    \item Insert CD-ROM, and run \file{CDEX.EXE} (or double-click it)
    \item Select a \sQuote{district}; press \sQuote{enter}
    \item Select a \sQuote{station}; press \sQuote{enter}
    \item Select \sQuote{Elements to Convert}, and select the desired
    fields using the \sQuote{space bar}; press \sQuote{enter}
    \item Change \sQuote{Drive/directory of output files} to a
    convenient location, for example \file{C:\\TEMP}
    \item Press \sQuote{F10} to extract the data (the name of the file
    is the 7-digit alphanumeric station number, followed by a
    \sQuote{.ALL} extension)
    \item Repeat these steps for each meteorological station desired (if
    there are more).
  }

  Multiple stations can be imported and combined before or after
  importing into \R. Multiple files can be concatenated into one from
  the system shell (e.g. DOS: \code{COPY *.ALL new.dly}, or UNIX:
  \code{cat *.ALL > new.dly}). This cleans up the \R workspace by only
  using one object to refer to several stations. Stations can be
  referred to functions in \pkg{seas} using their IDs.
  
  To import the archive file into \R:
  \enumerate{
    \item Start \R; type \code{library(seas)}
    \item Import using \code{dat <- read.msc("/temp/C1161661.ALL")}
    (note that \R uses forward slashes for directories, but you could
    alternatively type "C:\\\\TEMP\\\\C1161661.ALL" on a Microsoft-based
    platform to \sQuote{escape} the back slash characters)
  }

  To export the data from \R in a more convenient format for other programs,
  use \code{write.csv(dat,"out.csv")}; MS Excel users may want to turn
  \code{NA} values into the format recognized by Excel, so modify the
  expression to \code{write.csv(dat,"out.csv",na="#N/A")}.
}
\source{
  Data provided by the Meteorological Service of Canada
  (\url{http://www.msc.ec.gc.ca/}), with permission.
  
  This data may only be reproduced for personal use; any other
  reproduction is permitted only with the written consent of Environment
  Canada (\url{http://climate.weatheroffice.ec.gc.ca/contacts/}).
}
\seealso{\code{\link{read.msc}}}
\references{
  \url{http://climate.weatheroffice.ec.gc.ca/prods_servs/documentation_index_e.html} MSC archive format description

  \url{http://www.climate.weatheroffice.ec.gc.ca/prods_servs/cdcd_iso_e.html} CDCD CD-ROM download location

  \url{http://dosbox.sourceforge.net} for emulating DOS on non-Microsoft
  platforms

  \url{http://www.intevation.de/~bernhard/archiv/uwm/canadian_climate_cdformat/} an alternative method of extracting data from the CDCD CD-ROMs using a Python module by Bernhard Reiter
}
\author{M.W. Toews}
\examples{
file <- system.file("data/A1128551.DLY",package="seas")
print(file)
dat <- read.msc(file)
head(dat)
str(dat)

plot.seas.temp(dat)
plot.year(dat)
}
\keyword{datasets}
