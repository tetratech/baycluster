
#' @importFrom utils packageVersion

.onLoad <- function(libname = find.package("baycluster"), pkgname = "baycluster"){
  
  # declaration of global variables (http://stackoverflow.com/questions/9439256)
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("begin", "methodsList", "figNum"))
  invisible()
}

