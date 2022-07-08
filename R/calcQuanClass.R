# ####
#' @title Calculate and return quantile class by year and month
#' 
#' @description Calculate and return quantile class by year and month
#'   
#' @details 
#' Question: Should quantiles be computed with all data and then trim
#' un-used years data? Or should data be trimmed by year first? 
#' 
#' 
#' @param data Data table to analyze. Must have two columns: \code{dateCol} and
#'   \code{valueCol} which contain a date and value to analyze, respectively
#' @param dateCol Column name that contains date, e.g., \code{dateCol =
#'   SampleDate}
#' @param valueCol Column name that contains values to analyze for quantiles,
#'   e.g., \code{valueCol = Concentrations}
#' @param transform Log transform values before analysis
#' @param probabilities Vector of increasing probabilities to analyze. 
#' @param startYear First year to maintain in returned data set
#' @param endYear Last year to maintain in returned data set
#' @param yearType Indicate whether to perform analysis on a "calendar" or
#'   "water" year
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
#' 
#' @export
#' 
calcQuanClass <- function(data 
  , dateCol = date
  , valueCol = flow
  , transform = TRUE
  , probabilities = seq(0,1,0.25)
  , startYear
  , endYear
  , yearType = "calendar") { 
  
  # ----< testing >----
  {
    if (FALSE) {
      dateCol = date
      valueCol = flow
      transform = TRUE  # FALSE
      probabilities = seq(0,1,0.25)
      startYear = 1994
      endYear = 2020
      yearType = "calendar" # "water"
    }
  } # end ~ testing
  
  # ----< Error trap >----
  {
    # dateCol and valueCol must exist and be Date and numeric fields, respectively
    stopifnot(
      deparse(substitute(dateCol)) %in% names(data) 
      , deparse(substitute(valueCol)) %in% names(data)
      , lubridate::is.Date(dplyr::pull(data[ , deparse(substitute(dateCol))]))
      , is.numeric(dplyr::pull(data[ , deparse(substitute(valueCol))]))
      )
  } # end ~ error trap
  
  # ----< Set month processing order based on yearType >----
  {
  monOrder <- case_when(tolower(yearType)== "water" ~ c(10:12,1:9)
    , TRUE ~ 1:12)
  } # end ~ Set month processing order
  
  # ----< Create data set for analysis >----
  {
    data1 <- data %>%
      
      # down-select to columns with date and value
      select(., {{dateCol}}, {{valueCol}}) %>%
      
      # calculations
      mutate(.
        # extract year and month
        , year  = lubridate::year({{dateCol}})
        , month = lubridate::month({{dateCol}})
        
        # log transform if option is selected
        , value = case_when(transform ~ log({{valueCol}})
          , TRUE ~ {{valueCol}})) 
      
    # adjust year when water year selected
    if (tolower(yearType)== "water") {
      data1[,"year"] <- calcWaterYear(month, year)
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
  
} # end ~ function: calcQuanClass