# ####
#' @title Perform rudimentary data checking of variables in c.spec
#' 
#' @description Perform rudimentary data checking of variables in c.spec. Report
#'   out which variables should have been passed but not present.
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecChk <- function(c.spec) {
  
  # Cannot proceed without knowing which list of variables to check for ####
  stopifnot(c.spec$datSource %in% c("gam", "WRTDS", "file"))
  
  # ----< Extract variables to look for >----
  qc <- c.specQC %>%
    filter(., .data[[paste0("warn",toupper(c.spec$datSource))]]) %>%
    select(., Variable, Description, Type, Where)
  
  # ----< Check presence of variables in c.spec - warning only >----
  {
    # Check for whether variable was passed 
    ArgumentsNotPassed <- unlist(qc[!(qc$Variable %in% names(c.spec)), "Variable"])
    
    if (length(ArgumentsNotPassed) >0) {
      msg <- paste0("Following variables not passed to setSpec or stored in c.spec"
        , c.spec$datSource, ": "
        , vec.strg(ArgumentsNotPassed)
        , " \n -- further checking continues but you might need to address this issue")
      warning(simpleWarning(msg))
    }
  } # end ~ Check overall variables
  
  # ----< folder check >----
  {
    chkList <- qc %>%
      filter(., Type == "Folder") %>%
      select(., Variable) %>%
      unlist(.)
    
    for (chk in chkList) {
      if (not_empty(c.spec[[chk]]) ) {
        if (!file.exists(c.spec[[chk]])) {
          warning(simpleWarning(paste(chk," not found")))
        }
      } else {
        warning(simpleWarning(paste(chk," variable not provided but is needed")))
      }
    }
  } # end ~ folder check
  
  
  # ----< file check >----
  {
    chkList <- qc %>%
      filter(., Type == "File") %>%
      select(., Variable, Where) %>%
      as.data.frame(.)
    
    for (k1 in 1:NROW(chkList)) {
      cFolder <- chkList[k1,"Where"]
      cFile   <- chkList[k1,"Variable"]
      
      if ( not_empty(c.spec[[cFolder]]) && not_empty(c.spec[[cFile]]) ) {
        if (!file.exists(file.path(c.spec[[cFolder]], c.spec[[cFile]]))) {
          warning(simpleWarning(paste(cfile," not found")))
        }
      } else {
        warning(simpleWarning(paste(cfile," variable not provided but is needed")))
      }
    } # end ~ for (k1
    
  } # end ~ file check 
  
  # ----< start year >----
  {
    if (not_empty(c.spec$startYear) && not_empty(c.spec$endYear)) {
      if (!see_if(c.spec$startYear < c.spec$endYear)) 
      {
        warning(simpleWarning("startYear not less than endYear"))
      }
    } else {
      warning(simpleWarning("startYear and/or endYear variable not provided but is needed"))
    }
  }
  
  # ----< profVar and idVar >----
  {
    if ( not_empty(c.spec[["profVar"]]) && not_empty(c.spec[["idVar"]]) ) {
      if ( c.spec[["profVar"]] %in%  c.spec[["idVar"]]   )  {
        warning(simpleWarning(paste("Error: profVar variable",c.spec[["profVar"]],"also listed in idVar:",vec.strg(c.spec[["idVar"]] ))))
      }
    } else {
      warning(simpleWarning(paste("profVar and/or idVar"," variable not provided but is needed")))
    }
  }
  
  # ----< datSource specific checking >----
  if(c.spec$datSource == "gam") {
    setSpecChkGAM(c.spec) 
  } else if (c.spec$datSource == "WRTDS") {
    setSpecChkWRTDS(c.spec) 
  }
  

} # end ~ function: setSpecChk