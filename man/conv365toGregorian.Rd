\name{conv365toGregorian}
\alias{conv365toGregorian}
\title{Converts a data.frame using a 365-day calendar to a Gregorian calendar}
\description{
  Converts a \code{data.frame} with a 365-day calendar to a proleptic
  Gregorian calendar, repeating data from December 30th on a leap year
  to the remaining and missing December 31st.
}
\synopsis{
conv365toGregorian(x)
}
\usage{
conv365toGregorian(x)
}
\arguments{
  \item{x}{a \code{\link{data.frame}} with a \code{date} column}
}
\details{
  This function may be expanded in the future to be more flexible.
}
\value{
  Returns a \code{\link{data.frame}} with Gregorian calendar dates.
}
\author{M.W. Toews}
\keyword{utilities}
\keyword{datagen}
