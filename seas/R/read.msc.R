"read.msc" <- function(file, flags = FALSE, add.elem, format, verbose = TRUE) {
  if (!missing(format))
    warning("format parameter ignored, since the format is auto-detected")
  # Build internal element database elem.all
  # TODO: move it to a separate data resource, and expand for other elements
  deg <- iconv("\260","latin1","")
  deg <- if (is.na(deg)) "deg" else deg
  degC <- paste0(deg, "C")
  sq <- iconv("\262", "latin1","")
  sq <- if (is.na(sq)) "^2" else sq
  rad <- paste0("MJ/m", sq)
  pcp <- "mm"
  elem.all <- data.frame(
    alias=c(1, 2, 3, 10, 11, 12,
            13, 77, 123, 133, 61, 64),
    name=c(
      "t_max", "t_min", "t_mean", "rain", "snow", "precip",
      "snow_d", "press", "rain", "sun", "solar", "net_solar"),
    long.name=c(
      "daily maximum temperature",
      "daily minimum temperature",
      "daily mean temperature",
      "total rainfall",
      "total snowfall",
      "total precipitation",
      "snow on the groud",
      "station pressure",
      "hourly rainfall",
      "sunshine",
      "RF1 global solar radiation",
      "RF4 net-all wave radiation"),
    units=c(
      degC, degC, degC, "mm", "mm", "mm",
      "cm", "kPa", "mm", "hrs", rad, rad),
    scale=c(
      0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
      1.0, 0.01, 0.1, 0.1, 0.001, 0.001),
    stringsAsFactors=FALSE)

  if (!missing(add.elem)) {
    if (class(add.elem) %in% c("list", "data.frame"))
      elem.all <- rbind(elem.all, add.elem)
    else
      stop(gettextf("%s must either be a data.frame or a list",
                    sQuote("add.elem")))
  }

  orig <- as.character(substitute(file))[[1]]
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop(gettextf("%s must be a character string or connection",
                  sQuote(orig)))
  if (!isOpen(file)) {
    open(file, "r")
    on.exit(close(file))
  }
  rd <- readLines(file)  # read archive file

  # Use number of characters per line to auto-detect archive file format
  nchar_line <- unique(nchar(rd))
  if (length(nchar_line) != 1)
    stop("records are not fixed-width; this is possibly not a MSC archive file")
  if (nchar_line == 186)
    format <- "HLY"
  else if (nchar_line == 233)
    format <- "DLY"
  else if (nchar_line == 98) {
    format <- "MLY"
    stop(paste(
      "MLY archive format not supported at this time",
      "(Note: you can easily obtain DLY data and re-create the equivalent)",
      sep="\n"))
  } else if (nchar_line == 691) {
    format <- "FIF"
    stop("FIF archive format not supported at this time")
  } else
    stop("Format not recognized as supported archive format")

  # Check for duplicate records, and remove them with a warning
  dups <- duplicated(rd)
  sum_dups <- sum(dups)
  if (sum_dups > 0) {
    warning(gettextf("Removed %d duplicate records (%.2f%%) from archive file",
                     sum_dups, 100 * sum_dups / length(rd)))
    rd <- rd[!dups]
  }

  # Build data.frame from fixed-width substrings of archive file,
  # starting with columns: id, year, month, [day,], elem, more added later
  d  <- data.frame(id=factor(substr(rd, 1, 7)))
  if (verbose) {
    message(gettextf(
      "Format is %s with %d records from %d station%s",
      format, nrow(d), length(levels(d$id)),
      ifelse(length(levels(d$id)) > 1, "s", "")))
  }
  d$year <- as.integer(substr(rd, 8, 11))
  d$month <- as.integer(substr(rd, 12, 13))
  if (format=="HLY") {
    rep <- 24
    off <- 16
    d$day <- as.integer(substr(rd, 14, 15))
    d$date <- as.Date(paste(d$year, d$month, d$day, sep="-"))
    d$yday <- as.integer(format(d$date, "%j"))
  } else if (format=="DLY") {
    rep <- 31
    off <- 14
  }
  d$elem <- factor(substr(rd, off, off + 2))

  # Ensure all elements are recognized
  sel <- as.integer(levels(d$elem)) %in% elem.all$alias
  if (!all(sel))
    stop(paste(
      sum(!sel), "element",
      ngettext(sum(!sel), "alias", "aliases"),
      "are not recognized, and need to be expanded with add.elem:",
      paste(levels(d$elem)[!sel], collapse=", ")))
  elem.name <- elem.all$name[elem.all$alias %in% as.integer(levels(d$elem))]
  levels(d$elem) <- elem.name.orig <- elem.name
  if (verbose) {
    message(sprintf(ngettext(
      length(elem.name),
      "%d element found: %s",
      "%d elements found: %s"),
      length(elem.name), paste(elem.name, collapse=", ")))
    # Re-use a formatted string for later messages
    msg_fmt <- paste0("%8s: %", floor(log10(nrow(d)) + 1), "d records")
  }

  # With data.frame d, add 24 or 31 columns for each hour or day
  # For each element record, V_ for value and optionally F_ for flag
  if (flags) {
    elem.name <- paste0(rep(elem.name, each=2), c("", "_flag"))
    d[, paste0(c("V", "F"), rep(1:rep, each=2))] <- as.integer(NA)
  } else
    d[, paste0("V", 1:rep)] <- as.integer(NA)
  # Extract columns from archive file
  for (col in 1:rep) {
    voff <- off + 3 + (col - 1) * 7
    v <- as.integer(substr(rd, voff, voff + 5))
    v[v == -99999] <- NA
    d[, paste0("V", col)] <- v
    if (flags)
      d[, paste0("F", col)] <- substr(rd, voff + 6, voff + 6)
  }
  rm(rd)

  # Write to a temporary file, since it is faster to append rows (lines)
  # TODO: find a better way to do this
  tmp_fname <- tempfile(format, fileext=".csv")
  FILE <- file(tmp_fname, "w")
  # Write a header
  if (format == "HLY") {
    cat("id", "year", "yday", "hour", "date", "datetime", elem.name,
        sep=",", file=FILE)
    # column positions for V_ values and F_ flags for each hour of the day
    if (flags) {
      col.v <- (0:23) * 2 + 6
      col.f <- (0:23) * 2 + 7
    } else
      col.v <- (0:23) + 6
  } else if (format == "DLY") {
    cat("id", "year", "month", "day", elem.name,
        sep=",", file=FILE)
    # col.v and col.f are variable for each month, and are defined later
  }
  writeLines("", con=FILE)  # newline
  for (id in levels(d$id)) {
    # For each station ID: s
    s <- d[d$id == id,]
    if (verbose) {
      start <- min(s$year); end <- max(s$year)
      message(paste(
        gettextf(msg_fmt, id, nrow(s)),
        ifelse(
          start == end,
          gettextf("in %4d", start),
          gettextf("between %4d and %4d", start, end))))
    }
    for (year in unique(s$year)) {
      # For each year: ss
      ss <- s[s$year == year,]
      if (format == "HLY")
        row <- unique(ss$yday)
      else if (format == "DLY") {
        d.mon <- unclass(table(mkseas(width="mon", year=year)))
        row <- unique(ss$month)
      }
      for (col in row) {
        # Build m data.frames for each day-of-year / month ...
        if (format == "HLY") {
          # HLY: for each day-of-year, build 24-row df for each hour
          m <- data.frame(id=id, year=year, yday=col,
                          hour=0:23, date=NA, datetime=NA)
          qf <- ss$yday == col
        } else if (format == "DLY") {
          # DLY: for each month, build df with 28 to 31 rows for each day
          m <- data.frame(id=id, year=year, month=col, day=1:d.mon[col])
          qf <- ss$month == col
          # column positions for V_ values and F_ flags for each day of month
          if (flags) {
            col.v <- seq(1, d.mon[col] * 2, 2) + 4
            col.f <- seq(1, d.mon[col] * 2, 2) + 5
          } else
            col.v <- seq(1, d.mon[col], 1) + 4
        }
        # Attach empty columns for element data
        m[, elem.name] <- NA
        e <- as.character(ss[qf, "elem"])
        if (flags) {
          p <- t(ss[qf, col.v])
          f <- t(ss[qf, col.f])
          p <- cbind(p, f)
          e <- c(e, paste(e, "flag", sep="_"))
        } else
          p <- t(ss[qf, col.v])
        colnames(p) <- e
        if (length(e) > 0) {
          m[, e] <- p
          m <- m[apply(!is.na(m[, elem.name.orig, drop=FALSE]), 1, any),]
          write.table(m, FILE, append=TRUE, quote=FALSE, sep=",",
                      row.names=FALSE, col.names=FALSE)
        }
      }
    }
  }
  close(FILE)

  # Re-read temporary file
  if (format == "HLY")
    cc <- c("factor", "integer", "integer", "integer", "integer", "integer")
  else if (format == "DLY")
    cc <- c("factor", "integer", "integer", "integer")
  if (flags)
    cc <- c(cc, rep(c("integer", "factor"), length(elem.name.orig)))
  else
    cc <- c(cc, rep("integer", length(elem.name.orig)))
  o <- read.table(tmp_fname, sep=",", header=TRUE, colClasses=cc)
  unlink(tmp_fname)
  if (format == "HLY") {
    o$date <- as.Date(paste(o$year, o$yday), "%Y %j")
    o$datetime <- as.POSIXct(strptime(paste(o$date, o$hour), "%F %H"))
  } else if (format == "DLY") {
    o$day <- as.Date(paste(o$year, o$month, o$day, sep="-"))
    o$month <- as.integer(format(o$day, "%j"))
    n <- names(o)
    n[n %in% c("month", "day")] <- c("yday", "date")
    names(o) <- n
  }

  # Apply scale and add metadata for each element
  for (i in elem.name.orig) {
    info <- elem.all[elem.all$name %in% i,][1,]
    if (info$scale != 1)
      o[, i] <- o[, i] * info$scale
    attr(o[, i], "long.name") <- info$long.name
    if (!is.na(info$units))
      attr(o[, i], "units") <- info$units
  }
  o
}
