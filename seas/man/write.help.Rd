\name{write.help}
\alias{write.help}
\encoding{UTF-8}
\title{Write climate data in the format used by the HELP model}
\description{
  Write climate data in the format used by the Hydrological Evaluation
  of Landfill Performance (HELP) model. This exports the data using two
  slightly different variants of HELP: the DOS versions (3.07 to 3.80D)
  and for Visual HELP.
}
\usage{
write.help(file, dat, var = "", name = "", region, lat,
           visual.help = FALSE, metric = TRUE)
}
\arguments{
  \item{file}{name of output file; [DOS] HELP uses extensions
    \file{*.D4}, \file{*.D7}, and \file{*.D13} for daily precipitation,
    temperature and solar radiation, respectively; Visual HELP uses the
    file names \file{_weather1.dat}, \file{_weather2.dat} and
    \file{_weather3.dat} for the same series of variables}
  \item{dat}{\code{data.frame} of climate data}
  \item{var}{variable to be exported; must be one of \code{"precip"},
    \code{"t_mean"} or \code{"solar"}}
  \item{name}{\code{character}; location name}
  \item{region}{\code{character}; region}
  \item{lat}{\code{numeric}; location latitude in decimal degrees}
  \item{visual.help}{\link{logical} formats output for Visual HELP; else
    formated for the DOS HELP versions (default)}
  \item{metric}{\link{logical} if using metric units (this only sets a
    flag, please ensure the data are in either \enc{°}{degrees }C,
    mm/day and \eqn{\mbox{MJ}/(\mbox{m}^2 \cdot \mbox{day})}{MJ/(m^2*day)}
    or \enc{°}{degrees }F, in./day and langleys/day)}
}
\details{
  This utility function is experimental and has not been extensively
  tested; please report any errors to me.

  HELP requires continuous data; no missing values are allowed.

  Data imported from SDSM use a 365-day calendar, and can be
  approximated using \code{\link{conv365toGregorian}}.
}
\author{Mike Toews}
\references{
  HELP 3.07 - Original version for the US EPA; free download \cr
  \url{https://www.epa.gov/land-research/hydrologic-evaluation-landfill-performance-help-model}

  HELP-D - Developed by Dr. Klaus Berger, University
  of Hamburg \cr
  \url{https://www.geo.uni-hamburg.de/en/bodenkunde/service/help-model.html}

  Visual HELP - Uses a similar underlying code as HELP 3.07, but
  features a Windows GUI \cr
  \verb{https://www.waterloohydrogeologic.com/visual-help/} (dead link)
}
\seealso{\code{\link{read.msc}}, \code{\link{read.sdsm}},
  \code{\link{read.lars}}, \code{\link{conv365toGregorian}}}
\keyword{file}
\keyword{connection}
\keyword{utilities}
