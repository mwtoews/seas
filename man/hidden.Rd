\name{hidden}
\alias{hidden}
\alias{.seasxlab}
\alias{.seasylab}
\alias{.seastitle}
\alias{.seasmonthgrid}
\title{Return title and properties for seasonal graphs}
\description{
  Return title, x- and y-axis labels for seasonal
  graphs. Also, draw the month grid.
}
\usage{
.seasxlab(width,start.day)
.seasylab(var,long.name=NULL,units=NULL)
.seastitle(main=NULL, id=NULL, name=NULL,
           orig=NULL, fun=NULL, range=NA)
.seasmonthgrid(width, days, start=1, rep=0, start.day=1, month.label)
}
\arguments{
  \item{orig}{original object name, which is used if no other name can
    be found from \code{id} or \code{name}}
  \item{var}{original variable name in \code{names(orig)}}
  \item{width}{size of bin; see \code{\link{mkseas}}}
  \item{start.day}{the starting day of annum for the first bin; as
    either a \code{\link{Date}} or \code{\link{integer}} of day of the year}
  \item{main}{main title of plot; overrides any other title, but appends
    year range if \code{show.range=TRUE}}
  \item{long.name}{a long name for \code{var}, used for labelling y-axis}
  \item{units}{units of \code{var}, used for labelling y-axis}
  \item{id}{station ID, which is used to fetch a station name using
    \code{\link{getstnname}}}
  \item{name}{a name, which is used for labels}
  \item{fun}{function, if applicable}
  \item{range}{year range; \code{c(start.year,end.year)}}
  \item{days}{also known as \code{bin.lengths}, which represents the
    maximum number of days expected in each bin for a complete annum}
  \item{start}{the starting bin number}
  \item{rep}{the number of repeated bins}
  \item{month.label}{\code{\link{logical}}; put month name labels on grid}
}
\details{
  These functions are intended for producing the graphics, and do not
  need to be used directly.
  
  The month grid is drawn by \code{.seasmonthgrid}, and can be
  fine-tuned by setting some \link{options} in the \R environment, found in
  \code{seas.month.grid}. This \code{\link{list}}, for instance, has
  \code{len} to adjust the length of each month label, and \code{col}
  for the colour of the lines. See \code{\link{SeasOpts}} for all
  available options, and instructions on how to change them. 
  
  The main and variable/unit label formating can also be customized by
  setting other options, documented in \code{\link{SeasOpts}}.
}
\value{
  .seasxlab, .seasylab and .seastitle return a \code{character} label
  intended for plots.
}
\author{M.W. Toews}
\seealso{\code{\link{getstnname}}, \code{\link{SeasOpts}}}
\examples{
setSeasOpts()

.seasxlab(11,1)
.seasxlab("mon",1)

# Not starting on January 1st
.seasxlab(11,120)

# Labelled according to month (and possibly day)
getOption("seas.label")$month
.seasxlab("mon",as.Date("2000-08-01"))
getOption("seas.label")$monthday
.seasxlab(365/20,as.Date("2000-08-15"))
}
\keyword{utilities}
