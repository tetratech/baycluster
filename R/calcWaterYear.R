# ####
#' @title Calculate water year from given year and month 
#' 
#' @description Water years begin on October 1 and are accorded the year
#'   associated with the ending day of the water year. For example, October 1,
#'   2016 is in the 2017 water year.
#'   
#' @param month month must be from 1-12
#' @param year year
#' 
#' @examples 
#' # TBD
#' 
#' @return vector of water year
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' @export
#' 
calcWaterYear <- function(month,year) {
  
  # ----< Error trap >----
  {
    # correct type of file must be specified
    `%notin%` <- Negate(`%in%`)
    
    x<-stopifnot(
      month %notin% c(1:12)
    )
  } # end ~ error trap
  
  # ----< Calculate water year >----
  {
    year <- year + (month %in% c(10,11,12))
  }
  
  return(year)
  
} # end ~ function: calcWaterYear
