".First.lib" <- function(libname, pkgname) {
  library.dynam("seas", pkgname, libname)
  setSeasOpts()
}
".Last.lib" <- function(libpath){
  library.dynam.unload("seas",libpath)
}
