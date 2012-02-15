"getstnname" <-
  function(id) {
    suppressWarnings(data(mscstn)) # I'm not too sure about this line
    name <- mscstn$name[match(id,mscstn$nid)]
    if(length(name) == 0 || is.na(name)) name <- NULL
    name
  }

