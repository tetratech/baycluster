
#' @importFrom utils packageVersion

.onLoad <- function(libname = find.package("baycluster"), pkgname = "baycluster"){
  
  # declaration of global variables (http://stackoverflow.com/questions/9439256)
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("begin", "methodsList", "figNum"))
  invisible()
}

.onAttach <-  function(libname = find.package("baycluster")
  , pkgname = "baycluster"){
  # packageStartupMessage(paste0("**baycluster v",packageVersion("baycluster")
  #," Notice.** This software program is preliminary or provisional and is 
  # subject to revision. This software program is for testing only, no warranty
  # , expressed or implied, is made as to the accuracy and functioning of the 
  # program and related program material nor shall the fact of distribution 
  # constitute any such warranty, and no responsibility is assumed in connection
  # therewith. This software is provided 'AS IS.' "))
  packageStartupMessage(paste0("Loading:baycluster v"
    ,packageVersion("baycluster")))
}
