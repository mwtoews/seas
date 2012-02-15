\name{plot.seas.temp}
\alias{plot.seas.temp}
\encoding{latin1}
\title{Plot seasonal temperature normals}
\description{
  Plot seasonal temperature normals using boxplots, and also plot
  seasonal diurnal variability between minimum and maximum temperature.
}
\synopsis{
plot.seas.temp(x, width=11, start=1, rep=0, start.day=1,
           var=c("t_min","t_max","t_mean"),
           add.alt=FALSE, ylim, main, ylab, ...)
}
\usage{
plot.seas.temp(x)

plot.seas.temp(x, width=11, start=1, rep=0, start.day=1,
           var=c("t_min","t_max","t_mean"),
           add.alt=FALSE, ylim, main, ylab, \ldots)
}
\arguments{
  \item{x}{a \code{data.frame} with \code{\link{Date}}, \code{t_min},
    \code{t_max}, and (optionally) \code{t_mean} columns; \code{x} can
    also have \code{id} or \code{name} \link{attributes} to help give a
    title for the plot}
  \item{width}{size of bin; see \code{\link{mkseas}}}
  \item{start}{starting bin number; e.g., if \code{width="mon"} and
    \code{start=5}, the plot will start on "May" at the left-hand side}
  \item{rep}{repetition of the bins in the boxplots}
  \item{start.day}{if \code{width} is numeric, this is the day of year
    which is considered to be the start of the first bin}
  \item{var}{array specifying the names of the columns in \code{x}
    which relate to the \emph{minimum}, \emph{maximum} and \emph{mean}
    temperatures; the \code{units} attribute for the y-axis label are
    taken from the \code{minimum}, if available, otherwise it is
    assumed it is in \enc{°}{deg}C}
  \item{add.alt}{\code{logical}; add an alternative scale: if the units
    are in \enc{°}{deg}C, the alternative is \enc{°}{deg}F; if units are
    \enc{°}{deg}F, the alternative is \enc{°}{deg}C; and if units are K,
    the alternative is \enc{°}{deg}C}
  \item{ylim}{\code{c(min,max)} range for temperature, or y-axis}
  \item{main}{title for plot; if it is missing, then it will
    automatically be generated}
  \item{ylab}{y-axis label; if it is missing, then it will automatically
    be generated}
  \item{\ldots}{ignored}
}
\details{
  Plots boxplots for seasonal temperature normals from mean daily
  temperature, and diurnal variability with the mean difference of daily
  minimum and maximum temperatures (red vertical lines). If the mean is
  not supplied, it is calculated from the mean of daily maximum and
  minimum temperatures.
}
\value{
  Returns values from \code{\link{boxplot}} statistics on mean temperature.
}
\author{M.W. Toews}
\seealso{\code{\link{plot.seas.var}}, \code{\link{plot.seas.norm}},
  \code{\link{plot.year}}
  
  Use \code{mksub} to make a subset of \code{x}.
}
\examples{
data(mscdata)

dat <- mksub(mscdata,id=1108447)
plot.seas.temp(dat)
plot.seas.temp(dat, width="mon", add.alt=TRUE)

# starting and ending elsewhere
plot.seas.temp(dat, start=18, rep=3)
}
\keyword{hplot}
