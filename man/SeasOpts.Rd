\name{SeasOpts}
\alias{SeasOpts}
\alias{setSeasOpts}
\title{Options for seas}
\description{
  Set default options for \pkg{seas}.
}
\usage{
setSeasOpts()
}
\details{
  \code{setSeasOpts} sets all the default values for options in
  \pkg{seas}, and at some point it may support arguments for styles,
  such as \sQuote{black and white}. However, after the initial setting of
  options, users may change the options to modify the look of graphics
  produced in \pkg{seas}.

  Other details of the graphics can be modified using
  \code{\link{par}}. This includes the font sizes, back-ground colour,
  font family, and many others. For example, setting \code{par(cex=0.75)}
  will reduce the font size in the active device by  75\% of the
  original size; while \code{par(font.main=2)} will change only the font
  for the main titles.
}
\value{
  \code{setSeasOpts()} only sets the \code{options} in the current
  environment, and returns nothing.

  This is automatically done when \pkg{seas} is loaded (using
  \code{\link{.onLoad}}).
}
\section{Options used in seas}{
  Here are all the supported options for \pkg{seas}, with the default
  values shown for each option. Options are stored in
  \code{\link{list}}s, which make them easy to \sQuote{get}, but
  difficult to \sQuote{set}, and is shown in the
  \emph{Examples} section at the bottom.

  \describe{
    \item{\code{seas.main}:}{formating style for main title:
      \describe{
	\item{\code{fmt}:}{format for \code{name} and \code{id} (if
	  available) as the first \code{"\%s"}, followed by a range of
	  years as the second \code{"\%s"}; these are formated by
	  \code{\link{sprintf}}; \code{"\%s\n\%s"}}
	\item{\code{rngsep}:}{separation between ranges of years; \code{"-"},
	  other alternatives could be \code{" to "}}
	\item{\code{show.id}:}{show id (if available) in main title;
	  \code{TRUE}}
	\item{\code{show.fun}:}{show function (where applicable) in
	  main title; \code{TRUE}}
      }
    }
    \item{\code{seas.label}:}{label formating for variables:
      \describe{
	\item{\code{fmt}:}{label for name and units (if available);
	  \code{"\%s (\%s)"}, other alternatives could be
	  \code{"\%s, \%s"}}
	\item{\code{monthday}:}{format for month and day (see
	  \code{\link{strftime}} for format codes); this can be either
	  \code{"\%b \%-d"} (for most Unix-like systems),
	  \code{"\%b \%#d"} (for Windows systems), or \code{"\%b \%d"}
	  (for other systems); this should produce a string, such as
	  \sQuote{Aug 1} for \emph{August 1st}}
	\item{\code{month}}{similar as previous, but when starting
	  exactly on month-breaks; \code{"\%B"}}
	\item{\code{ann}}{a label for \code{\link{image.seas.sum}};
	  default is \sQuote{annual}}
	}
    }
    \item{\code{seas.month.grid}:}{setting for the display of the
      month grid (see \code{\link{.seasmonthgrid}}), which is common to
      many plots that use a \code{\link{numeric}} \code{width} in
      \code{\link{mkseas}}:
      \describe{
	\item{\code{abb}:}{abbreviate month names for grid; \code{TRUE}}
	\item{\code{len}:}{trim month name lengths to a number, for
	  instance to get J|F|M|A|M|J|J|A|S|O|N|D, use \code{1}; \code{NULL}}
	\item{\code{force}:}{force the display of each month label using
	  \code{\link{mtext}}, otherwise labels can be automatically placed
	  and adjusted for device using \code{\link{axis}}; \code{TRUE}}
	\item{\code{label}:}{show a month label on the grid; \code{TRUE}}
	\item{\code{col}:}{colour for month grid; \code{"lightgrey"}}
	\item{\code{lwd}:}{width for month grid lines, multiplied by
	  \code{par("lwd")}; \code{1}}
	\item{\code{lty}:}{style for month grid lines; \code{1}}
      }
    }
    \item{\code{seas.bxp}:}{attributes which affect the display of
      boxplots, used by various functions:
      \describe{
	\item{\code{boxcol}:}{default box-fill colour; \code{"lightgrey"}}
	\item{\code{outcex}:}{outlier symbol size, multiplied by
	  \code{par("cex")}; \code{1}}
      }
    }
    \item{\code{seas.temp}:}{attributes which affect the display of
      \code{\link{seas.temp.plot}} (among other functions):
      \describe{
	\item{\code{col}:}{colours for boxplot fill and diurnal variability
	  lines; \code{c("lightgrey","red")}}
	\item{\code{lwd}:}{width of diurnal variability lines in
	  \code{seas.temp.plot}, multiplied by \code{par("lwd")};
	  \code{3}}
      }
    }

    \item{\code{seas.precip}:}{attributes which affect the display of
      precipitation:
      \describe{
	\item{\code{col}:}{colour; \code{"grey"}}
	\item{\code{density}:}{pattern density; \code{NULL}}
	\item{\code{angle}:}{pattern angel; \code{45}}
	\item{\code{lwd}:}{thickness of box line, multiplied by
	    \code{par("lwd")}; \code{1}}
      }
    }
    \item{\code{seas.rain}:}{attributes which affect the display of
      rain:
      \describe{
	\item{\code{col}:}{colour; \code{"lightblue"}}
	\item{\code{density}:}{pattern density; \code{NULL}}
	\item{\code{angle}:}{pattern angel; \code{45}}
	\item{\code{lwd}:}{thickness of box line, multiplied by
	    \code{par("lwd")}; \code{1}}
      }
    }
    \item{\code{seas.snow}:}{attributes which affect the display of
      snow:
      \describe{
	\item{\code{col}:}{colour; \code{"lightgrey"}}
	\item{\code{density}:}{pattern density; \code{NULL}}
	\item{\code{angle}:}{pattern angel; \code{-45}}
	\item{\code{lwd}:}{thickness of box line, multiplied by
	    \code{par("lwd")}; \code{1}}
      }
    }
    \item{\code{seas.interarrival}:}{attributes which affect the display
      of wet- and dry-spells in \code{\link{plot.interarrival}};
      organized as \code{c(wet,dry)}:
      \describe{
	\item{\code{col}:}{colour; \code{c("lightblue","orange")}}
      }
    }
    \item{\code{seas.median}:}{attributes which affect the display of
      the \code{\link{median}} lines in \code{\link{image.seas.sum}}:
      \describe{
	\item{\code{col}:}{colour; \code{"red"}}
	\item{\code{lwd}:}{width of line, multiplied by
	    \code{par("lwd")}; \code{1}}
	\item{\code{lty}:}{style of line; \code{1}}
      }
    }
    \item{\code{seas.mean}:}{attributes which affect the display of the
      \code{\link{mean}} lines in \code{\link{image.seas.sum}}:
      \describe{
	\item{\code{col}:}{colour; \code{"red"}}
	\item{\code{lwd}:}{width of line, multiplied by
	    \code{par("lwd")}; \code{1}}
	\item{\code{lty}:}{style of line; \code{1}}
      }
    }
    \item{\code{seas.na}:}{attributes which affect the display of
      \code{NA} or missing values in various plots:
      \describe{
	\item{\code{col}:}{colour; \code{"red"}}
	\item{\code{pch}:}{character symbol; \code{"x"}}
      }
    }
  }
}
\author{Mike Toews}
\seealso{\code{\link{hidden}}}
\examples{
if(is.null(getOption("seas.main")))
  setSeasOpts()

# Modify an option
getOption("seas.main")$show.id
cp <- orig <- getOption("seas.main")
cp$show.id <- FALSE
options(seas.main=cp)
getOption("seas.main")$show.id

options(seas.main=orig)
}
\keyword{utilities}
