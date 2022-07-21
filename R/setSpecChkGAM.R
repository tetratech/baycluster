# ####
#' @title Perform GAM-specific data checking of c.spec
#' 
#' @description Perform GAM-specific data checking of c.spec
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
setSpecChkGAM <- function(c.spec) {
  

  # ----< monthGrid check >----
  if ( not_empty(c.spec[["monthGrid"]]) ) {
    if (any(!(c.spec[["monthGrid"]] %in% 1:12))) {
      warning(simpleWarning(paste("Invalid monthGrid: must be 1-12")))
    }
  } else {
    warning(simpleWarning(paste("monthGrid"," variable not provided but is needed")))
  }
  
  
  # ----< FUTURE DEVELOPMENT: Notify user of 1 wqParm limitation >----
  if ( not_empty(c.spec[["wqParm"]]) ) {
    if (length(c.spec[["wqParm"]]) > 1) {
      warning(simpleWarning(paste("Ok to include more than one water quality parameter for the moment,\n"
      , " but we'll downselect to the first item for cluster analysis later.")))
    }
  } else {
    warning(simpleWarning(paste("wqParm"," variable not provided but is needed")))
  }
  
  # ----< FUTURE DEVELOPMENT: Notify user of 1 wqLayer limitation >----
  if ( not_empty(c.spec[["wqLayer"]]) ) {
    if (length(c.spec[["wqLayer"]]) > 1) {
      warning(simpleWarning(paste("Ok to include more than one layer for the moment,\n"
        , " but we'll downselect to the first item for cluster analysis later.")))
    }
  } else {
    warning(simpleWarning(paste("wqLayer"," variable not provided but is needed")))
  }

  
} # end ~ function: setSpecChkGAM