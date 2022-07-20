# ####
#' @title Complete c.spec with WRTDS-specific supporting variables
#' 
#' @description Complete c.spec with WRTDS-specific supporting variables
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom tibble tibble as_tibble 
#' @importFrom scales col_numeric
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecCmpWRTDS <- function(c.spec) {
  

  # ----< TBD >----
  {
    # to be developed
  }
   
  return(c.spec)
  
} # end ~ function: setSpecCmpWRTDS