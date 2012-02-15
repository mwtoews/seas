\name{dathomog}
\alias{dathomog}
\title{Homogenize daily data sets}
\description{
  Homogenizes daily data from two data sets into one data set;
  optionally show cross-plots to examine how well correlated that data
  sets are.
}
\usage{
dathomog(x1, x2, by = "date", plot = FALSE)
}
\arguments{
  \item{x1}{a \code{data.frame} of seasonal data; 1st selection}
  \item{x2}{a \code{data.frame} or seasonal data; 2nd selection}
  \item{by}{name of common column, usually \sQuote{date}, which is of
    class \code{\link{Date}}}
  \item{plot}{\code{logical}; produce cross-plots and correlation
    statistics of the variables between the two data sets}
}
\details{
  Data from \code{x1} has priority over \code{x2}.  Where data from
  \code{x1} is either \code{NA} or missing (outside of time range), data
  from \code{x2} will be used (if available). Otherwise data form
  \code{x1} will be used directly. Variables will be
  homogenized where their names are identical, found using
  \code{\link{names}}.
  
  The cross-plots of the data are shown only for interest. They show
  useful correlation statistics, and a best-fit line using perpendicular
  offsets (which are preferred in this case over traditional linear
  regression). At some point, the equations for this line may be used to
  adjust the values from \code{x2}, however this can always be done
  externally to this function by pre-processing \code{x2}.
}
\value{
  Returns a \code{data.frame} of seasonal data required by most functions
  in \pkg{seas}. Variable names of the structure are found by a
  \code{\link{union}} of the names of \code{x1} and \code{x2}.
}
\references{http://mathworld.wolfram.com/LeastSquaresFittingPerpendicularOffsets.html}
\author{M.W. Toews}
\section{Warning}{
  Weather stations should be sufficiently close enough to approximate
  the same weather. This distance depends on the spatial distance and
  local climatology.
}
\examples{
data(mscdata)
dat1 <- mksub(mscdata,id=2100630)
dat2 <- mksub(mscdata,id=1108447)
plot.year(dat1)
plot.year(dat2)
newdata <- dathomog(dat1,dat2)
plot.year(newdata)
message(paste(c("This is a rather poor example, since the",
                "two stations are nowhere near each other"),
                collapse="\n"))
}
\keyword{utilities}
\keyword{datagen}
