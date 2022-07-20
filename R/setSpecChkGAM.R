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
  
  
} # end ~ function: setSpecChkGAM