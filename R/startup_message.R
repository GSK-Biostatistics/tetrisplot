#' To remove on first major release 
.onAttach <- function(libname, pkgname) { 
 packageStartupMessage("This is the development version of tetrisplot")
} 