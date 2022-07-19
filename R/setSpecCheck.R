# ####
#' @title Perform rudimentary data checking of c.spec
#' 
#' @description Perform rudimentary data checking of c.spec
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return n/a
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom assertthat not_empty see_if
#' 
#' @export
#' 
setSpecCheck <- function(c.spec) {
  
  # Check overall variables - warning only ####
  {
    x <- c.specQC
    
    # Check for whether variable was passed 
    ArgumentsNotPassed <- unlist(x[!(x$Variable %in% names(c.spec)), "Variable"])
    
    if (length(ArgumentsNotPassed) >0) {
      msg <- paste("Following variables not passed to setSpec or stored in c.spec:"
        , vec.strg(ArgumentsNotPassed)
        , " \n -- further checking continues but you might need to address this issue")
      warning(simpleWarning(msg))
    }
    
  } 
  
  # datSource check ####
  if (not_empty(c.spec$datSource) ) {
    if (!see_if(c.spec$datSource %in% c("gam", "WRTDS", "file")))
    {
      warning(simpleWarning("datSource not in gam, WRTDS, file"))
    }
  } else {
    warning(simpleWarning("datSource not provided but is needed"))
  }
  
  # start year ####
  if (not_empty(c.spec$startYear) && not_empty(c.spec$endYear)) {
    if (!see_if(c.spec$startYear < c.spec$endYear)) 
    {
      warning(simpleWarning("startYear not less than endYear"))
    }
  } else {
    warning(simpleWarning("startYear and/or endYear variable not provided but is needed"))
  }
  
  # folder check ####
  chkList <- c("projFolder", "gamFolder", "datFolder", "outFolder", "exCovFolder")
  
  for (chk in chkList) {
    if (not_empty(c.spec[[chk]]) ) {
      if (!file.exists(c.spec[[chk]])) {
        warning(simpleWarning(paste(chk," not found")))
      }
    } else {
      warning(simpleWarning(paste(chk," variable not provided but is needed")))
    }
  }
  
  # station file check ####
  if ( not_empty(c.spec[["datFolder"]]) && not_empty(c.spec[["statFile"]]) ) {
    if (!file.exists(file.path(c.spec[["datFolder"]], c.spec[["statFile"]]))) {
      warning(simpleWarning(paste("statFile"," not found")))
    }
  } else {
    warning(simpleWarning(paste("statFile"," variable not provided but is needed")))
  }
  
  
  
  # monthGrid check ####
  if ( not_empty(c.spec[["monthGrid"]]) ) {
    if (any(!(c.spec[["monthGrid"]] %in% 1:12))) {
      warning(simpleWarning(paste("Invalid monthGrid: must be 1-12")))
    }
  } else {
    warning(simpleWarning(paste("monthGrid"," variable not provided but is needed")))
  }
  
  # profVar and idVar ####
  if ( not_empty(c.spec[["profVar"]]) && not_empty(c.spec[["idVar"]]) ) {
    if ( c.spec[["profVar"]] %in%  c.spec[["idVar"]]   )  {
      warning(simpleWarning(paste("Error: profVar variable",c.spec[["profVar"]],"also listed in idVar:",vec.strg(c.spec[["idVar"]] ))))
    }
  } else {
    warning(simpleWarning(paste("profVar and/or idVar"," variable not provided but is needed")))
  }
  
  
}





