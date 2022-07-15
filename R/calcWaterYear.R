# ####
#' @title Calculate water year from given year and month 
#' 
#' @description Water years begin on October 1 and are accorded the year
#'   associated with the ending day of the water year. For example, October 1,
#'   2016 is in the 2017 water year.
#'   
#' @param yearIn year
#' @param monthIn month must be from 1-12
#' 
#' @examples 
#' \dontrun{
#' dat0 <- tidyr::tibble( a = seq(as.Date('2022-06-10'), as.Date('2023-03-01'), by = "30 days"))
#' 
#' dat0 <- calcYearMonth(dat0
#'   , dateCol = "a"
#'   , yearCol = "Year"
#'   , monthCol = "theMonth")
#' 
#' dat0$WY <- calcWaterYear(dat0$Year, dat0$theMonth)
#' }
#' 
#' @return vector of water year
#' 
#' @seealso \code{\link{readTextFile}} \code{\link{calcYearMonth}}
#' 
#' @export
#' 
calcWaterYear <- function(yearIn, monthIn) {
  
  # yearIn = data1$year; monthIn=data1$month
  # tmp <- tibble(year = yearIn, month = monthIn)
  
  # ----< Error trap >----
  {
    
    # stop if months from 1:12
    stopifnot(
      monthIn %in% 1:12
    )
  } # end ~ error trap
  
  # ----< Calculate water year >----
  {
    yearOut <- yearIn + monthIn %in% 10:12
  }
  
  return(yearOut)
  
} # end ~ function: calcWaterYear
