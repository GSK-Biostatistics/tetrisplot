#' To remove on first major release 
#' @keywords internal
.onAttach <- function(libname, pkgname) { 
 packageStartupMessage("This is the development version of tetrisplot")
} 