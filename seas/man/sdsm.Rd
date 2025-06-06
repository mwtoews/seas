\name{sdsm}
\alias{sdsm}
\alias{read.sdsm}
\alias{write.sdsm}
\title{Read and write from SDSM}
\description{
  Reads and writes the data format used in
  SDSM's \file{DAT} and \file{OUT} extensions.
}
\usage{
# reading
read.sdsm(file, start = 1961, end = 2000, calendar)

# writing
write.sdsm(dat, var, start, end, file = "")
}
\arguments{
  \item{file}{name of \file{DAT} or \file{OUT} file}
  \item{dat}{\code{data.frame} of variables to be written}
  \item{start}{starting year}
  \item{end}{ending year}
  \item{var}{name of variable to be written from \code{dat}}
  \item{calendar}{calendar used for data; if unspecified, this is
    assumed to be proleptic Gregorian (normal); however, for CCCma
    models this should be "365_day", and for Hadley models this should
    be "360_day"; see \code{\link{year.length}}}
}
\details{
  This function readings and writes climate data with the
  Statistical Downscaling Model, or SDSM. The model uses \file{DAT}
  extensions for input data, such as daily observations of mean
  temperature, and \file{OUT} extensions for modeled output.
}
\value{
  \code{read.sdsm} returns a \code{data.frame} of the measured
  variables. The variables are named \code{V1}\ldots\code{Vn},
  for \emph{n} ensembles.

  If a \code{calendar} is specified, this is stored as an attribute in
  the \code{date} data frame column.
}
\author{Mike Toews}
\references{
  Wilby, R.L., Dawson, C.W. and Barrow, E.M. 2002.
  SDSM --- a decision support tool for the assessment of regional
  climate change impacts, \emph{Environmental Modelling Software},
  \bold{17} (2), 145--157, \doi{10.1016/S1364-8152(01)00060-3}

  SDSM can be downloaded free-of-charge for Windows platforms from \cr
  \url{https://www.sdsm.org.uk/}

  CGCM1 and HADCM3 model data for SDSM can be downloaded from the
  Canadian Climate Impacts and Scenarios website: \cr
  \url{https://web.archive.org/web/20120218192015/http://www.cics.uvic.ca/scenarios/sdsm/select.cgi} (archived)
}
\seealso{\code{\link{read.msc}}, \code{\link{change}}}
\examples{
\dontrun{
# reading
fname <- system.file("extdata", "GF_2050s_precip.OUT", package="seas")
gf50 <- read.sdsm(fname)
gf50.ss <- seas.sum(gf50, var=paste("V", 1:20, sep=""), name="Grand Forks")

# analysis
image(gf50.ss, var="V1")
image(gf50.ss, var="V2")
image(gf50.ss, var="V3")

# writing
data(mscdata)
hj <- mksub(mscdata, id=2100630)
fname <- paste(tempdir(), "HJ_Obs_prcp.DAT", sep="/")
write.sdsm(hj, "precip", 1961, 2000, fname)
}
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
