# ####
#' @title Calculate and return quantile class by year and month
#' 
#' @description Calculate and return quantile class by year and month
#'   
#' @details 
#' Quantiles are computed using all provided data, i.e., long-term
#' quantiles. The \code{startYear} and \code{endYear} limit the returned
#' data table to the desired year. 
#' 
#' Setting \code{probabilities} to \code{seq(0, 1, length.out = 5)} results in
#' probabilities being set to 0, 0.25, 0.50, 0.75, and 1.00; This leads to four
#' (4) classes being assigned where the leftmost interval corresponds to level
#' one, the next leftmost to level two and so on. Setting \code{length.out = 6}
#' will yield probabilities of 0, 0.2, 0.4, 0.6, 0.8, 1.0.
#' 
#' 
#' @param data Data table to analyze. Must have two columns: \code{dateCol} and
#'   \code{valueCol} which contain a date and value to analyze, respectively
#' @param dateCol Column name that contains date
#' @param valueCol Column name that contains values for analyzing for quantiles
#' @param transform Log transform values before analysis
#' @param numClasses Number of classes for computing quantiles 
#' @param startYear First year to maintain in returned data set
#' @param endYear Last year to maintain in returned data set
#' @param yearType Indicate whether to perform analysis on a "calendar" or
#'   "water" year
#' @param report Indicate whether to print table
#'  
#' @examples 
#' # TBD
#' 
#' @return data table with cross tabulation of quantiles by year and month
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom rlang .data
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 

#' 
#' @export
#' 
calcQuanClass <- function(data 
  , dateCol = "date"
  , valueCol = "flow"
  , transform = TRUE
  , numClasses = 4
  , startYear
  , endYear
  , yearType = "calendar"
  , report = TRUE) { 
  
  # ----< testing >----
  {
    if (FALSE) {
      dateCol = "date"
      valueCol = "flow"
      transform = TRUE  # FALSE
      numClasses = 4
      startYear = 1994
      endYear = 2020
      yearType = "calendar" # "water"
    }
  } # end ~ testing
  
  # ----< Error trap >----
  {
    # dateCol and valueCol must exist and be Date and numeric fields, respectively
    stopifnot(
      dateCol %in% names(data) 
      , valueCol %in% names(data)
      , is.Date(pull(data[ , dateCol]))
      , is.numeric(pull(data[ , valueCol]))
      )
  } # end ~ error trap
  
  # ----< Compute even sized probability classes >----
  {
    probabilities <- seq(0, 1, length.out = numClasses+1)
  }
  
  # ----< Set month processing order based on yearType >----
  {
  monOrder <- case_when(tolower(yearType)== "water" ~ c(10:12,1:9)
    , TRUE ~ 1:12)
  } # end ~ Set month processing order
  
  # ----< Create data set for analysis >----
  {
    data1 <- data %>%
      
      # down-select to columns with date and value
      select(., .data[[dateCol]], .data[[valueCol]] ) %>%
      
      # calculations
      mutate(.
        # extract year and month
        , year  = year(.data[[dateCol]])
        , month = month(.data[[dateCol]])
        
        # log transform if option is selected
        , value = case_when(transform ~ log(.data[[valueCol]])
          , TRUE ~ .data[[valueCol]])) 
    
    # adjust year when water year selected
    if (tolower(yearType)== "water") {
      data1 <- calcWaterYear(data1, "year", "month", "year")
    }
    
  } # end ~ Create data set for analysis
  
  # ----< Process data at annual level >----
  {
    # Average data by year ####
    ffyr <- data1 %>%
      group_by(., year) %>%
      summarise(., avg = mean(value))
    
    # Compute annual quantiles ####
    quanVal <- quantile(ffyr$avg, probabilities)
    quanVal <- quanVal + c(-0.1, rep(0,length(quanVal)-2), 0.1) # extend end points slightly
    
    # Add quantile class to annual data set ####
    ffyr <- ffyr %>%
      mutate(., YearCat = cut(avg, quanVal, labels = FALSE))
  } # end ~ Process data at annual level
  
  # ----< Process data at monthly level >----
  {
    # Average data by year+month ####
    ffmo <- data1 %>%
      group_by(., year, month) %>%
      summarise(., avg = mean(value), .groups = "keep") %>%
      ungroup(.)

    # process each month ####  
    for(mon in monOrder) {
      
      # extract one month
      ff1mo <- ffmo %>%
        filter(., month == mon)
      
      # compute monthly quantiles
      quanVal <- quantile(ff1mo$avg, probabilities)
      quanVal <- quanVal + c(-0.1, rep(0,length(quanVal)-2), 0.1) # extend end points slightly
      
      # Add quantile class to monthly data set
      ff1mo <- ff1mo %>%
        mutate(.
          , !!month.abb[mon] := cut(ff1mo$avg, quanVal, labels = FALSE)) %>%
        select(., year, month.abb[mon])
  
      # Merge one month data results to overall data set    
      ffyr <- ffyr %>%
        left_join(., ff1mo, by = "year")
      
    } # end ~ for mon
  } # end ~ Process data at monthly level

  # ----< Filter final data set down to selected years >----
  {
  ffyr <- ffyr %>%
    filter(., year >= startYear
      , year <= endYear) %>%
    rename(., Year = year
      , YearAvg = avg)
  } # end ~ Filter final data set down
  
  # ----< Add attributes to final data set >----
  {
    ffyr <- ffyr %>%
      structure(out.attrs = NULL
        , numClasses = numClasses
        , monOrder   = monOrder
        , yearType   = yearType
        , transform  = transform
        , valueCol   = valueCol
        , dateCol    = dateCol)
  } # end ~ add attributes
  
  return(ffyr)
  
} # end ~ function: calcQuanClass