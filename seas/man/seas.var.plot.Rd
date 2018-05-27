\name{seas.var.plot}
\alias{seas.var.plot}
\alias{plot.seas.var}
\encoding{UTF-8}
\title{Plot seasonal normals of a given variable}
\description{
  Plot seasonal normals of a variable using boxplots.
}
\usage{
seas.var.plot(x, var, width = 11, start = 1, rep = 0, start.day = 1,
              col, ylim, add.alt, alt.ylab, main, ylab, ylog, \ldots)
}
\arguments{
  \item{x}{a \code{data.frame} with \code{\link{Date}} and \code{var}
    columns of data; \code{x} can also have \code{id} or \code{name}
    \link{attributes} to help give a title for the plot}
  \item{var}{a variable; a column name in \code{x}; this can also have
    attributes of \code{units} and \code{long.name} to help give a title
    for the y-axis}
  \item{width}{size of bin; see \code{\link{mkseas}}}
  \item{start}{starting bin number; e.g., if \code{width="mon"} and
    \code{start=5}, the plot will start on "May" at the left-hand side}
  \item{rep}{repetition of the bins in the boxplot}
  \item{start.day}{when \code{width} is numeric, this is the starting
    day of the year for the fist bin, or it can be a \code{Date} to
    specify a month and day (year is ignored)}
  \item{col}{colour for the boxplots; the default is \code{"lightgrey"}}
  \item{ylim}{\code{c(min, max)} range for y-axis}
  \item{add.alt}{this adds an alternative axis, and is specified by
    \code{c(slope,inter)}; for example, if the primary measure is in
    \enc{°}{degrees }C, a secondary scale in K would be
    \code{c(1,273.15)}, or in \enc{°}{degrees }F would be
    \code{c(5/9,32)}; if \code{ylog=TRUE}, then this can also be
    \code{TRUE} to display the \eqn{\log_{10}}{log10} transformed values of
    \code{var} on the alternative axis}
  \item{alt.ylab}{label for the alternate y-axis (the primary y-axis
    label is set through \link{attributes} for \code{var} in \code{x})}
  \item{main}{title for plot; if it is missing, then it will
    automatically be generated}
  \item{ylab}{y-axis label; if it is missing, then it will automatically
    be generated}
  \item{ylog}{used to \eqn{\log_{10}}{log10} transform values of
    \code{var} for the boxplots; this has a similar but different affect
    than specifying \code{par(ylog=TRUE)} before this function}
  \item{\ldots}{ignored}
}
\details{
  Shows normals of a seasonal variable using boxplots.
}
\note{
  This function was formerly named \code{plot.seas.var}, but required
  renaming as it is not an S3 method.
}
\value{
  Returns values from \code{\link{boxplot}} statistics on the variable.
}
\author{Mike Toews}
\seealso{
  \code{\link{seas.var.plot}}, \code{\link{plot.seas.norm}},
  \code{\link{year.plot}}.

  Use \code{\link{mksub}} to make a subset of \code{x}.
}
\examples{
opar <- par(no.readonly=FALSE)
on.exit(par(opar))
data(mscdata)
dat <- mksub(mscdata, id=1108447)

seas.var.plot(dat, var="t_max", col="tomato",
  add.alt=c(5/9, 32), alt.ylab="F")
abline(h=0)

par(opar) # reset graphics parameters

seas.var.plot(dat, var="t_min",
  start=18, rep=16)

pdat <- dat[dat$precip > 0,]
attr(pdat$precip, "long.name") <- "precipitation intensity"
attr(pdat$precip, "units") <- "mm/day"

par(ylog=TRUE)
seas.var.plot(pdat, var="precip", col="azure")
title(sub="These boxplots are simply plotted on a log-y scale")

par(opar)

seas.var.plot(pdat, var="precip", col="azure", ylog=TRUE)
title(sub="These boxplots are based on log-transformed values")

seas.var.plot(pdat, var="precip", col="azure", ylog=TRUE, add.alt=TRUE)
title(sub="The actual axis for graph is on the right-side")
}
\keyword{hplot}
