# ####
#' @title Add water year from given year and month to a data table
#' 
#' @description Water years begin on October 1 and are accorded the year
#'   associated with the ending day of the water year. For example, October 1,
#'   2016 is in the 2017 water year.
#'   
#' @param data Data table to analyze. Must have two column: \code{yearCol} and
#'   \code{monthCol}
#' @param yearCol Column name that contains year data
#' @param monthCol Column name that contains month data
#' @param wateryearCol Column name to store water year. If left as NA, then
#'   yearCol will be overwritten.
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
#' dat0 <- calcWaterYear(dat0, "Year", "theMonth", "WY")
#' }
#' 
#' @return vector of water year
#' 
#' @seealso \code{\link{readTextFile}} \code{\link{calcYearMonth}}
#' 
#' @export
#' 
calcWaterYear <- function(data
  , yearCol = "year"
  , monthCol = "month"
  , wateryearCol = NA) {
  
  # ---- < set wateryearCol to overwrite yearCol if not provided >---
  {
    if (any(is.na(wateryearCol))) {
      wateryearCol = yearCol
    }
  }

  # ----< Error trap >----
  {
    # yearCol and monthCol must exist in data
    stopifnot(
      yearCol %in% names(data) 
      , monthCol %in% names(data) 
      , unlist(data[ , monthCol]) %in% 1:12
    )
  } # end ~ error trap
  
  
  # ----< Calculate water year >----
  {
    data[ , wateryearCol] <- data[ , yearCol] + unlist(data[ , monthCol]) %in% 10:12
  }
  
  return(data)
  
} # end ~ function: calcWaterYear
