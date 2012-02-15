\name{change}
\alias{change}
\title{Find seasonal and annual changes between two data sets}
\description{
  Find seasonal and annual changes between two data sets; relative and
  absolute changes are found between the central tendency and spread of
  each seasonal state.
}
\synopsis{
change(x1, x2, var1, var2=var1, width="mon",
       cent="mean",sprd="sd", disc=FALSE, inter=FALSE,
       p.cut=0.3, start.day=1, calendar)
}
\usage{
# minimum usage
change(x1, x2, var1)

# all options
change(x1, x2, var1, var2=var1, width="mon",
       cent="mean",sprd="sd", disc=FALSE, inter=FALSE,
       p.cut=0.3, start.day=1, calendar)
}
\arguments{
  \item{x1}{a \code{data.frame} of seasonal data}
  \item{x2}{a second \code{data.frame} of seasonal data}
  \item{var1}{a variable in \code{x1}}
  \item{var2}{a variable in \code{x2}}
  \item{width}{the width of the bins, see \code{\link{mkseas}} for more
    details; this will change the sample-sizes between \code{x1} and
    \code{x2}, which can affect the changes detected}
  \item{cent}{a \code{\link{function}} to find a central tendency;
    usually this is \code{\link{mean}}, however \code{\link{median}} or
    other functions can be used too}
  \item{sprd}{a \code{function} to find a spread around a central
    tendency; usually this will be \code{\link{sd}}
    (standard-deviation), however \code{\link{mad}} or other functions
    can be used too}
  \item{disc}{if the data are discontinuous, the \code{seas.sum} objects
    are created for \code{var1}/\code{var2} to determine the changes;
    this is ideal for precipitation, and other sparsely distributed
    variables}
  \item{inter}{\code{\link{interarrival}}s are calculated, and changes
    are found between \emph{wet} and \emph{dry} series}
  \item{p.cut}{cut-off for wet/dry; see \code{\link{interarrival}}}
  \item{start.day}{starting day}
  \item{calendar}{calendar; if not specified it will try to read this
    from the attributes, otherwise it is assumed to be a proleptic
    Gregorian calendar; see \code{\link{year.length}}}
}
\details{
  This function is useful for finding changes between different states of
  seasonal data. Here, a \emph{state} represents how seasonal data
  behave statistically at either a \emph{time} or
  \emph{place}. The stability of a state depends on both the variance
  throughout each portion of the season, as well as the number of years
  of observations.

  For instance, seasonal and annual changes in climate can be detected
  in climate data series, by comparing the normals from two time periods.
}
\value{
  Returns a complex \code{\link{list}} of relative and absolute (if
  applicable) changes of \code{var1}/\code{var2} between \code{x1} and
  \code{x2}.

  Seasonal and annual changes are identified independently of each
  other; where annual changes have a \code{ann} prefix.

  Relative changes are not found if \code{x$var} has values less than 0,
  such as Temperature measured in degrees C or F.
}
\seealso{\code{\link{dathomog}}, \code{\link{lars}}}
\author{M.W. Toews}
\examples{
data(mscdata)
dat1 <- mksub(mscdata,id=1108447,start=1975,end=1984)
dat2 <- mksub(mscdata,id=1108447,start=1985,end=1995)

# A few plot functions to make thing easy
plot.ch <- function(x,main,h,col){
  main <- paste(main,"between 1975-1984 and 1985-1994",sep="\n")
  barplot(x,main=main)
  abline(h=c(0,h),col=c(1,col),lty=c(1,2))
}
plot.abs <- function(x,col="red",abs="abs",ann.abs="ann.abs"){
  main <- sprintf("Absolute change in \%s",x$long.name[[1]])
  plot.ch(x[[abs]],main,x[[ann.abs]],col)
}
plot.rel <- function(x,col="orange",rel="rel",ann.rel="ann.rel"){
  main <- sprintf("Relative change in \%s",x$long.name[[1]])
  plot.ch(x[[rel]],main,x[[ann.rel]],col)
}
plot.std <- function(x,col="purple"){
  main <- sprintf("Relative change in the\nstandard deviation of \%s",
                  x$long.name[[1]])
  plot.ch(x$sprd.rel,main,x$ann.sprd.rel,col)
}

# Minimum temperature
ch <- change(dat1,dat2,"t_min")
str(ch)
plot.abs(ch)
plot.std(ch)
# Cannot do ch$rel ; since div/0!

# Precipitation
ch2 <- change(dat1,dat2,"precip",width="DJF",disc=TRUE)
plot.abs(ch2,"blue")
plot.rel(ch2,"purple")
plot.std(ch2)


}
\keyword{utilities}
\keyword{datagen}
