"getstnname" <-
function(id) {
    # Lookup MSC station data from internal data.frame
    idx <- match(id, seas::mscstn$nid)
    name <- seas::mscstn$name[idx]
    name
}
