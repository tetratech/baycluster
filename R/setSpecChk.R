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
  stopifnot(c.spec$dat_source %in% c("gam", "WRTDS", "file"))
  
  # ----< Extract variables to look for >----
  qc <- c.spec_qc %>%
    filter(., .data[[paste0("warn",toupper(c.spec$dat_source))]]) %>%
    select(., Variable, Description, Type, Where)
  
  # ----< Check presence of variables in c.spec - warning only >----
  {
    # Check for whether variable was passed 
    ArgumentsNotPassed <- unlist(qc[!(qc$Variable %in% names(c.spec)), "Variable"])
    
    if (length(ArgumentsNotPassed) >0) {
      msg <- paste0("Following variables not passed to setSpec or stored in c.spec"
        , c.spec$dat_source, ": "
        , vec.strg(ArgumentsNotPassed)
        , " \n -- further checking continues but you might need to address this issue")
      warning(simpleWarning(msg))
    }
  } # end ~ Check overall variables
  
  # ----< folder check >----
  {
    chk_list <- qc %>%
      filter(., Type == "Folder") %>%
      select(., Variable) %>%
      unlist(.)
    
    for (chk in chk_list) {
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
    chk_list <- qc %>%
      filter(., Type == "File") %>%
      select(., Variable, Where) %>%
      as.data.frame(.)
    
    for (k1 in 1:NROW(chk_list)) {
      cFolder <- chk_list[k1,"Where"]
      cFile   <- chk_list[k1,"Variable"]
      
      if ( not_empty(c.spec[[cFolder]]) && not_empty(c.spec[[cFile]]) ) {
        if (!file.exists(file.path(c.spec[[cFolder]], c.spec[[cFile]]))) {
          warning(simpleWarning(paste(cfile," not found")))
        }
      } else {
        warning(simpleWarning(paste(cFile," variable not provided but is needed")))
      }
    } # end ~ for (k1
    
  } # end ~ file check 
  
  # ----< start year >----
  {
    if (not_empty(c.spec$start_year) && not_empty(c.spec$end_year)) {
      if (!see_if(c.spec$start_year < c.spec$end_year)) 
      {
        warning(simpleWarning("start_year not less than end_year"))
      }
    } else {
      warning(simpleWarning("start_year and/or end_year variable not provided but is needed"))
    }
  }
  
  # ----< prof_var and id_var >----
  {
    if ( not_empty(c.spec[["prof_var"]]) && not_empty(c.spec[["id_var"]]) ) {
      if ( c.spec[["prof_var"]] %in%  c.spec[["id_var"]]   )  {
        warning(simpleWarning(paste("Error: prof_var variable",c.spec[["prof_var"]],"also listed in id_var:",vec.strg(c.spec[["id_var"]] ))))
      }
    } else {
      warning(simpleWarning(paste("prof_var and/or id_var"," variable not provided but is needed")))
    }
  }
  
  # ----< dat_source specific checking >----
  if(c.spec$dat_source == "gam") {
    setSpecChkGAM(c.spec) 
  } else if (c.spec$dat_source == "WRTDS") {
    setSpecChkWRTDS(c.spec) 
  }
  

} # end ~ function: setSpecChk
