\name{summerland}
\alias{summerland}
\alias{summerland.sr}
\alias{summerland.dat}
\docType{data}
\title{Example LARS-WG data file of synthetic data from Summerland, BC}
\description{
  Example LARS-WG data file of synthetic data from Summerland, BC.
}
%\usage{data(mscdata)}
\format{
  Both files are ASCII-based, and can be viewed in any text editor
  \itemize{
    \item \file{summerland.sr} is the \sQuote{site file}, which contains
    the meta-data
    \item \file{summerland.dat} is the data file
  }
  Details of these file formats can be found in the LARS-WG manual and
  help documentation.
}
\details{
  The sample file name was generated in LARS-WG from calibration of data
  from Summerland (MSC ID: 1127800). Thirty-years were generated, each
  synthetic year has 365-days.
}
\seealso{\code{\link{read.lars}}, which contains an example using
  these files}
\author{Mike Toews}
\keyword{datasets}
