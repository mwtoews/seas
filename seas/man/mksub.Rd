\name{mksub}
\alias{mksub}
\title{Make a subset of seasonal data}
\description{
  Creates a subset of a \code{data.frame} with temporal observations,
  using IDs and start and ending dates or years.
}
\usage{
  mksub(x, start, end, id)
}
\arguments{
  \item{x}{a data frame with temporal observations}
  \item{start}{either a starting \code{\link{Date}} or
    \code{\link{integer}} year; if omitted minimum will be used}
  \item{end}{either an ending \code{Date} or year; if omitted will use
    same year as \code{start}, and if \code{start} is omitted, will use
    maximum year}
  \item{id}{unique station identifier (if present), which is assumed to
    be a column of \code{x} as \code{x$id}; it is used to extract a
    subset of data from a single ID}
}
\details{
  This utility function is useful for creating temporal subsets of
  seasonal data and for extracting a single station out of a
  \code{data.frame} with multiple stations or sets. The \code{x}
  object can have many columns, representing measured variables for each
  day, which will be returned with their original \link{attributes}.

  If \code{id} is used, that station will be extracted from
  \code{x}. If \code{id} is not provided, but there are more than one
  unique IDs in \code{x$id}, the first unique ID will be extracted, with
  a \link{warning}.
}
\value{
  Returns a subset of a \code{\link{data.frame}} with the same columns
  and attributes as \code{x}, except \code{id}, which will be retained
  as an attribute (e.g., \code{attr(x,"id")}).
}
\author{Mike Toews}
\seealso{\code{\link{read.msc}}, \code{\link{mscdata}}}
\examples{
data(mscdata)

# All available data from one station
summary(mksub(mscdata, id=1108447))

# One year
str(mksub(mscdata, id=1108447, start=1980))

# A range of years
str(mksub(mscdata, id=1108447, start=1980, end=1989))

# A range of dates
summary(mksub(mscdata, id=1108447,
              start=as.Date("1975-08-01"),
              end=as.Date("2000-07-31")))
}
\keyword{datagen}
\keyword{utilities}
