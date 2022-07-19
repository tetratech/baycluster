# ####
#' @title Create base prediction data set for GAM clusters
#' 
#' @description Constructs the base prediction data set for GAM clusters and
#'   includes dates, days of year, decimal year, etc.
#'   
#' @details 
#' Set \code{monthGrid = 1:12} to include all months of the year. \code{monthGrid
#' = 6:9} would only include June through September.  
#' 
#' Set \code{dayGrid = 15} to set up data set for the 15th of every month.
#' \code{dayGrid = c(10,20)} would set up data set for the 10 and 20th of every month.
#' 
#' Set \code{monthAdj = NA} results in analysis performed on a calendar year.
#' Setting \code{monthAdj = c(10, 11, 12)} would result in analysis being set
#' up for a water year basis, making October 1 the first day of the year. This
#' is accomplished by computing year.adj as the calendar year plus 1 ("+1") for
#' months 10-12. If \code{monthAdj = c(-1, -2, -3)} (note the negative values),
#' then year.adj is set to the calendar year minus 1 ("-1") for months 1-3. This
#' has the effect of making April 1 the first day of the year in this example.
#' 
#' @param startYear Begin year of analysis (scalar)
#' @param endYear End year of analysis (scalar)
#' @param monthGrid vector of months to include in analysis
#' @param dayGrid days of month to make predictions 
#' @param monthAdj month adjustment to accommodate water year analyses
#'  
#' @examples 
#' \dontrun{
#' basePred <- createBasePred(startYear = 2015
#'  , endYear = 2016
#'  , monthGrid = 1:12
#'  , dayGrid = c(10,20)
#'  , monthAdj = c(10,11,12))
#' }
#' 
#' @return data table with base prediction data set
#' \itemize{
#' \item year - year (calendar basis)   
#' \item month - month   
#' \item day - day of month   
#' \item date - date   
#' \item dyear - date expressed as decimal year   
#' \item doy - day of year (calendar basis)   
#' \item year.adj - year (adjusted based on monthAdj)   
#' \item doy.adj - day of year (adjusted based on year.adj)  
#' }
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 
#' 
#' @export
#' 
createBasePred <- function(startYear = 1990
  , endYear = year(Sys.Date())
  , monthGrid = 1:12
  , dayGrid = 15
  , monthAdj = NA) {
  
  # ----< Create base prediction data set >----
  
  # make a grid of year | month | day of month ####
  {
    # Include an additional year at the beginning and end to account for alternative
    # year starts
    data <- 
      as_tibble(expand.grid((startYear-1):(endYear+1), monthGrid, dayGrid)) %>%
      rename(., year = Var1, month = Var2, day = Var3) %>%
      
      # create date-related fields 
      mutate(.
        
        # compute date, decimal date, and day of year
        , date  = ymd(paste(year, month, day, sep = "-")) # date
        , dyear = decimal_date(date)                      # decimal year
        , doy   = yday(date)                              # day of year
        
        , year.adj = year
        , doy.adj = doy
      ) %>%
      arrange(., date)
  } # end ~ make a grid of year ...
  
  # Make year.adj for alternative year types ####
  {
    # advance year by 1 for all months listed in monthAdj assuming first monthAdj
    # is positive; otherwise decrease year by 1
    # monthAdj <- c(10, 11, 12) # good for water year
    # monthAdj <- c(-1, -2, -3) # good for growing season year  
    if (exists("monthAdj") && !(any(is.na(monthAdj)) | is.null(monthAdj))) {
      data <- data %>%
        mutate(.
          , year.adj = case_when(
            monthAdj[1] > 0 ~ year + month %in% abs(monthAdj)
            , monthAdj[1] < 0 ~ year - month %in% abs(monthAdj)
            , TRUE ~ year 
          )
        )
    }
    
    # Trim data set for year | calc adjust doy
    {
      data <- data %>%
        filter(., between(year.adj , startYear , endYear))
    }
    
  } # end ~ Make year.adj for alternative year types
  
  # Make doy.adj for alternative year types ####
  {
    data <- data %>%
      filter(., between(year.adj , startYear , endYear))
    
    # ** do not combine with above because unique(month) cannot be calculated
    data <- data %>%
      mutate(.
        , d1  = as.numeric(date - make_date(year, unique(month)[1], 1) )+1
        , d2 = as.numeric(date - make_date(year-1, unique(month)[1], 1))+1
        , doy.adj = case_when( between(d1,1,366) ~ d1
          , between(d2,1,366) ~ d2
          , TRUE ~ NA_real_)
      ) %>%
      select(., -d1, -d2)
  } # end ~ Make doy.adjustments for alternative year types
  
  # Add attributes documenting prediction data set creation ####
  {
    data <- data %>%
      structure( out.attrs = NULL
        , dateCreated = Sys.time()
        , startYear = startYear
        , endYear = endYear
        , monthGrid = monthGrid
        , dayGrid = dayGrid
        , monthAdj = monthAdj
        , firstDay = floor_date(min(data$date), unit = "month")
        , lastDay = ceiling_date(max(data$date), unit = "month")-1
        , month.order = unique(data$month)
      )
  } # end ~ Add attributes documenting prediction data set creation
  
  return (data)
  
}




