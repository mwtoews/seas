\name{sdsm}
\alias{sdsm}
\alias{read.sdsm}
\alias{write.sdsm}
\title{Read and write from SDSM}
\description{
  Reads and writes the data format used in
  SDSM's \file{DAT} and \file{OUT} extensions.
}
\synopsis{
read.sdsm(file, start = 1961, end = 2000, calendar)
write.sdsm(dat, var, start, end, file = "")
}
\usage{
# reading
read.sdsm(file, start = 1961, end = 2000, calendar)
read.sdsm(file)

# writing
write.sdsm(dat, var, start, end, file)
write.sdsm(dat, var)
}
\arguments{
  \item{file}{name of \file{DAT} or \file{OUT} file}
  \item{dat}{\code{data.frame} of variables to be written}
  \item{start}{starting year}
  \item{end}{ending year}
  \item{var}{name of variable to be written from \code{dat}}
  \item{calendar}{calender used for data; if unspecified, this is
    assumed to be proleptic Gregorian (normal); however, for CCCma
    models this should be "365\_day", and for Hadley models this should
    be "360\_day"; see \code{\link{year.length}}}
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
\author{M.W. Toews}
\references{
  Wilby, R.L., Dawson, C.W. and Barrow, E.M. 2002,
  \sQuote{SDSM - a decision support tool for the assessment of regional
    climate change impacts}, \emph{Environmental Modelling Software},
  \bold{77}, 473--471.
  
  SDSM can be downloaded free-of-charge for Windows platforms from \cr
  \url{http://www-staff.lboro.ac.uk/~cocwd/SDSM/}
  
  CGCM1 and HADCM3 model data for SDSM can be downloaded from the
  Canadian Climate Impacts and Scenarios website: \cr
  \url{http://www.cics.uvic.ca/scenarios/sdsm/select.cgi}
}
\seealso{\code{\link{read.msc}}, \code{\link{change}}}
\examples{
\dontrun{
# reading
gf50 <- read.sdsm("GF_2050s_precip.OUT")
gf50.ss <- seas.sum(gf50,var=paste("V",1:20,sep=""),name="Grand Forks")
# analysis
image(gf50.ss,var="V1")
image(gf50.ss,var="V2")
image(gf50.ss,var="V3")

# writing
data(mscdata)
hj <- mksub(mscdata,id=2100630)
write.sdsm(hj,"precip",1961,2000,"HJ_Obs_prcp.DAT")
}
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
