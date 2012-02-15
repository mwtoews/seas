\name{seas.check}
\alias{seas.df.check}
\alias{seas.sum.check}
\title{Check the suitability of a data.frame, or seas.sum for seas}
\description{
  Check the suitability of a \code{\link{data.frame}} or
  \code{\link{seas.sum}} object for \pkg{seas}.
}
\usage{
seas.df.check(x, orig, var)
seas.sum.check(x, orig, var, norm, year.filter, ann.only)
}
\arguments{
  \item{x}{a data frame with temporal observations}
  \item{orig}{the original name of the data frame, for error messages}
  \item{var}{one or more variables in \code{x}, which are tested;
    ignored if \code{NULL} or \link{missing}}
  \item{norm}{something to normalize \code{var}; it can either be the
    name of an array in \code{x}, a \code{\link{matrix}} (\code{bin} vs
    \code{years}), or a 3-dim \code{\link{array}} (used to normalize
    multiple \code{var}); this is tested to see if \code{norm} exists,
    and that the dimension are consistent with \code{x}}
  \item{year.filter}{a subset of \code{x$years}, which filters all the
    related arrays in the returned value}
  \item{ann.only}{\code{x$seas} arrays are ignored}
}
\details{
  This utility function simply checks the suitability of a
  \code{\link{data.frame}} or \code{\link{seas.sum}} objects for use with
  \pkg{seas}.

  If \code{x} is \code{data.frame} (using \code{seas.df.check} that is
  really required, is a \sQuote{date} column, named \code{x$date} with a
  \link{class} of either \code{link{POSIXct}} or \code{link{Date}}, and
  one or more variables in the \code{var} columns of \code{x}.

  There must be at least one finite observation in each of \code{var},
  if supplied.

  These function is used within other functions, and is not intended to
  be called directly.
}
\section{Value}{
  \code{seas.df.check} returns a few helpful items from \code{x} in a
  \code{\link{list}} using \code{\link{invisible}}:
  
  \itemize{
    \item{\code{id}:}{station ID from one of \code{attr(x,"id")} or
      \code{x$id[1]}}
    \item{\code{name}:}{name of seasonal data, such as a place}
    \item{\code{year.range}:}{integers of start, and ending years}
    \item{\code{calendar}:}{an attribute from \code{x$date};
      otherwise this will be \code{NULL} for a normal proleptic
      Gregorian calendar}
    \item{\code{main}:}{main title, from \code{\link{.seastitle}}}
    \item{\code{units}:}{units for \code{var[1]}}
    \item{\code{long.name}:}{long name for \code{var[1]}}
    \item{\code{ylab}:}{y-axis label for \code{var[1]}}

  }
  
  \code{seas.sum.check} returns \code{x} with modifications, depending
  on \code{norm} and \code{year.filter}.
}
\author{M.W. Toews}
\seealso{\code{\link{hidden}} functions for \pkg{seas}}
\examples{
data(mscdata)
dat <- mksub(mscdata, id=1108447)
str(seas.df.check(dat))

dat.ss <- seas.sum(dat)
str(seas.sum.check(dat.ss,norm="days"))
}
\keyword{utilities}
