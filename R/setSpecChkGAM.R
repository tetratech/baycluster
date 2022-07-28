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
  

  # ----< month_grid check >----
  if ( not_empty(c.spec[["month_grid"]]) ) {
    if (any(!(c.spec[["month_grid"]] %in% 1:12))) {
      warning(simpleWarning(paste("Invalid month_grid: must be 1-12")))
    }
  } else {
    warning(simpleWarning(paste("month_grid"," variable not provided but is needed")))
  }
  
  
  # ----< FUTURE DEVELOPMENT: Notify user of 1 wq_parm limitation >----
  if ( not_empty(c.spec[["wq_parm"]]) ) {
    if (length(c.spec[["wq_parm"]]) > 1) {
      warning(simpleWarning(paste("Ok to include more than one water quality parameter for the moment,\n"
      , " but we'll downselect to the first item for cluster analysis later.")))
    }
  } else {
    warning(simpleWarning(paste("wq_parm"," variable not provided but is needed")))
  }
  
  # ----< FUTURE DEVELOPMENT: Notify user of 1 wq_layer limitation >----
  if ( not_empty(c.spec[["wq_layer"]]) ) {
    if (length(c.spec[["wq_layer"]]) > 1) {
      warning(simpleWarning(paste("Ok to include more than one layer for the moment,\n"
        , " but we'll downselect to the first item for cluster analysis later.")))
    }
  } else {
    warning(simpleWarning(paste("wq_layer"," variable not provided but is needed")))
  }

  
} # end ~ function: setSpecChkGAM