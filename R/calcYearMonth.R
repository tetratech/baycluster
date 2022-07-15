# ####
#' @title Add year and month to data table
#' 
#' @description Based on a data table with a date field, add year and month 
#'   fields to the table.
#'   
#' @param data Data table to analyze. Must have one column: \code{dateCol} which
#'   contains a date to analyze 
#' @param dateCol Column name that contains date
#' @param yearCol Column name to store year data
#' @param monthCol Column name to store month data
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
#' @return data table with values for year and month added 
#' 
#' @seealso \code{\link{readTextFile}} \code{\link{calcWaterYear}}
#' 
#' @export
#' 
calcYearMonth <- function(data
  , dateCol = "date"
  , yearCol = "year"
  , monthCol = "month") {
  
  # ----< testing >----
  {
    if (FALSE) {
      dateCol = "date"
      yearCol = "year"
      monthCol = "month"
    }
  } # end ~ testing
  
  # ----< Error trap >----
  {
    # dateCol must exist and be Date field 
    stopifnot(
      dateCol %in% names(data) 
      , lubridate::is.Date(dplyr::pull(data[ , dateCol]))
    )
  } # end ~ error trap
  
  # ----< Create data set for analysis >----
  {
    data <- data %>%
      
      # calculations
      mutate(.
        , {{yearCol}} := lubridate::year(.data[[dateCol]])
        , {{monthCol}} := lubridate::month(.data[[dateCol]])
        )
    
  } # end ~ Create data set for analysis
  
  return(data)
  
} # end ~ function: calcYearMonth