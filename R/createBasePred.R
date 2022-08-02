# ####
#' @title Create base prediction data set for GAM clusters
#' 
#' @description Constructs the base prediction data set for GAM clusters and
#'   includes dates, days of year, decimal year, etc.
#'   
#' @details 
#' Set \code{month_grid = 1:12} to include all months of the year. \code{month_grid
#' = 6:9} would only include June through September.  
#' 
#' Set \code{day_grid = 15} to set up data set for the 15th of every month.
#' \code{day_grid = c(10,20)} would set up data set for the 10 and 20th of every month.
#' 
#' Set \code{month_adj = NA} results in analysis performed on a calendar year.
#' Setting \code{month_adj = c(10, 11, 12)} would result in analysis being set
#' up for a water year basis, making October 1 the first day of the year. This
#' is accomplished by computing year_adj as the calendar year plus 1 ("+1") for
#' months 10-12. If \code{month_adj = c(-1, -2, -3)} (note the negative values),
#' then year_adj is set to the calendar year minus 1 ("-1") for months 1-3. This
#' has the effect of making April 1 the first day of the year in this example.
#' 
#' @param start_year Begin year of analysis (scalar)
#' @param end_year End year of analysis (scalar)
#' @param month_grid vector of months to include in analysis
#' @param day_grid days of month to make predictions 
#' @param month_adj month adjustment to accommodate water year analyses
#'  
#' @examples 
#' \dontrun{
#' basePred <- createBasePred(start_year = 2015
#'  , end_year = 2016
#'  , month_grid = 1:12
#'  , day_grid = c(10,20)
#'  , month_adj = c(10,11,12))
#' }
#' 
#' @return data table with base prediction data set
#' \itemize{
#' \item year - year (calendar basis) 
#' \item month - month (Jan, Feb, ...)   
#' \item month_num - month (1, 2, 3, ...)  
#' \item day - day of month   
#' \item date - date   
#' \item dyear - date expressed as decimal year   
#' \item doy - day of year (calendar basis)   
#' \item year_adj - year (adjusted based on month_adj)   
#' \item doy_adj - day of year (adjusted based on year_adj)  
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
createBasePred <- function(start_year = 1990
  , end_year = year(Sys.Date())
  , month_grid = 1:12
  , day_grid = 15
  , month_adj = NA) {
  
  # ----< Create base prediction data set >----
  
  # make a grid of year | month | day of month ####
  {
    # Include an additional year at the beginning and end to account for alternative
    # year starts
    data <- 
      as_tibble(expand.grid((start_year-1):(end_year+1), month_grid, day_grid)) %>%
      rename(., year = Var1, month_num = Var2, day = Var3) %>%
      
      # create date-related fields 
      mutate(.
        
        # add month
        , month = month.abb[month_num]
        
        # compute date, decimal date, and day of year
        , date  = ymd(paste(year, month_num, day, sep = "-")) # date
        , dyear = decimal_date(date)                      # decimal year
        , doy   = yday(date)                              # day of year
        
        , year_adj = year
        , doy_adj = doy
      ) %>%
      arrange(., date)
  } # end ~ make a grid of year ...
  
  # Make year_adj for alternative year types ####
  {
    # advance year by 1 for all months listed in month_adj assuming first month_adj
    # is positive; otherwise decrease year by 1
    # month_adj <- c(10, 11, 12) # good for water year
    # month_adj <- c(-1, -2, -3) # good for growing season year  
    if (exists("month_adj") && !(any(is.na(month_adj)) | is.null(month_adj))) {
      data <- data %>%
        mutate(.
          , year_adj = case_when(
            month_adj[1] > 0 ~ year + month_num %in% abs(month_adj)
            , month_adj[1] < 0 ~ year - month_num %in% abs(month_adj)
            , TRUE ~ year 
          )
        )
    }
    
    # Trim data set for year | calc adjust doy
    {
      data <- data %>%
        filter(., between(year_adj , start_year , end_year))
    }
    
  } # end ~ Make year_adj for alternative year types
  
  # Make doy_adj for alternative year types ####
  {
    data <- data %>%
      filter(., between(year_adj , start_year , end_year))
    
    # ** do not combine with above because unique(month_num) cannot be calculated
    data <- data %>%
      mutate(.
        , d1  = as.numeric(date - make_date(year, unique(month_num)[1], 1) )+1
        , d2 = as.numeric(date - make_date(year-1, unique(month_num)[1], 1))+1
        , doy_adj = case_when( between(d1,1,366) ~ d1
          , between(d2,1,366) ~ d2
          , TRUE ~ NA_real_)
      ) %>%
      select(., -d1, -d2)
  } # end ~ Make doy_adjustments for alternative year types
  
  # Add attributes documenting prediction data set creation ####
  {
    data <- data %>%
      structure( out.attrs = NULL
        , date_created = Sys.time()
        , start_year = start_year
        , end_year = end_year
        , month_grid = month_grid
        , day_grid = day_grid
        , month_adj = month_adj
        , first_day = floor_date(min(data$date), unit = "month_num")
        , last_day = ceiling_date(max(data$date), unit = "month_num")-1
        , month_order = unique(data$month_num)
      )
  } # end ~ Add attributes documenting prediction data set creation
  
  return (data)
  
}




