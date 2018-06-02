\name{seas-package}
\alias{seas}
\docType{package}
\title{Seasonal statistics: the `seas' package for R}
\description{
The \code{seas} package for the \R programming environment is capable of
conveying descriptive statistics and graphics for seasonal variables, as found
in climatology, hydrology and ecology. Seasonal variables can be continuous
(i.e., temperature) or discontinuous (i.e., precipitation). An annum can be
partitioned into many arbitrary divisions, or seasonal components, such as
\emph{by month} or into other fixed intervals. Boxplots are used to describe
the seasonal distributions of continuous variables.  Discontinuous variables
need to be summed over time to smooth the irregularities before the variable
can evaluated and visualized. Statistics, such as precipitation normals, may
be derived from the summed variables, using the mean or median methods. Other
tools and utilities provided in the package can calculate precipitation
interarrivals, cumulative precipitation departures, find changes between two
normals, and import data from archive formats.

To get started, try some examples, such as \code{\link{seas.sum}},
\code{\link{seas.norm}}. There will be more help added here someday!

Source code and issues are at \url{https://github.com/mwtoews/seas}
}
\references{
  Toews, M.W., Whitfield, P.H., and Allen, D.M. 2007. Seasonal
  statistics: The `seas' package for R. \emph{Computers & Geosciences},
  \bold{33} (7), 944--951, \doi{10.1016/j.cageo.2006.11.011}
}
\author{Mike Toews}
\examples{
# Show a list of changes to the package:
file.show(system.file("ChangeLog", package="seas"))
}
\keyword{package}
