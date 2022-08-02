# ####
#' @title Calculate and return quantile class by year and month
#' 
#' @description Calculate and return quantile class by year and month
#'   
#' @details 
#' Quantiles are computed using all provided data, i.e., long-term
#' quantiles. The \code{start_year} and \code{end_year} limit the returned
#' data table to the desired years. 
#' 
#' Setting \code{num_classes = 4} results in probabilities being set to 0, 0.25,
#' 0.50, 0.75, and 1.00 for purposes of classification. This leads to four (4)
#' classes being assigned where the leftmost interval corresponds to level one
#' and the lowest numerical values, the next leftmost to level two and so on.
#' Setting \code{num_classes = 5} will yield probabilities of 0, 0.2, 0.4, 0.6,
#' 0.8, 1.0.
#' 
#' @param data Data table to analyze. Must have two columns: \code{date_col} and
#'   \code{value_col} which contain a date and value to analyze, respectively
#' @param date_col Column name that contains date
#' @param value_col Column name that contains values for analyzing for quantiles
#' @param transform_type if \code{transform_type = "logtransform"}, then values are
#'   log transformed before analysis
#' @param num_classes Number of classes for computing quantiles 
#' @param start_year First year to maintain in returned data set
#' @param end_year Last year to maintain in returned data set
#' @param month_adj Adjustment to months, setting to \code{month_adj = c(10,11,12)} is
#'   equivalent to calling for water year. 
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
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 

#' 
#' @export
#' 
calcQuanClass <- function(data 
  , date_col = "date"
  , value_col = "flow"
  , transform_type = NA
  , num_classes = 4
  , start_year
  , end_year 
  , month_adj = NA
  , report = TRUE) { 
  

  # ----< Error trap >----
  {
    # date_col and value_col must exist and be Date and numeric fields, respectively
    stopifnot(
      date_col %in% names(data) 
      , value_col %in% names(data)
      , is.Date(pull(data[ , date_col]))
      , is.numeric(pull(data[ , value_col]))
      )
  } # end ~ error trap
  
  # ----< Compute equal-sized probability classes >----
  {
    probabilities <- seq(0, 1, length.out = num_classes+1)
  }
  
  # ----< Set month processing order based on month_adj >----
  {
    # initialize
    month_order <- month_vec <- 1:12
    
    # process if there are any non-NA month_adj values
    if (any(!is.na(month_adj))) {
      if (month_adj[1] > 0) {
        month_order <- c(month_vec[month_vec %in% month_adj], month_vec[!(month_vec %in% month_adj)])
      } else {
        month_order <- c(month_vec[!(month_vec %in% abs(month_adj))], month_vec[(month_vec %in% abs(month_adj))])
      }
    }
    
  } # end ~ Set month processing order
  
  # ----< Create data set for analysis >----
  {
    data1 <- data %>%
      
      # down-select to columns with date and value
      select(., .data[[date_col]], .data[[value_col]] ) %>%
      
      # calculations
      mutate(.
        # extract year and month
        , year  = year(.data[[date_col]])
        , month = month(.data[[date_col]])
      )
    
    # transform if option is selected
    if (!is.na(transform_type)) {
      data1 <- transformData(data1, value_col, transform_type)
    }

    # adjust year when month_adj is specified  
    if (any(!is.na(month_adj))) {
      if (month_adj[1] > 1) {
        data1[ , "year"] <- data1[ , "year"] + unlist(data1[ , "month"]) %in% month_adj
      } else {
        data1[ , "year"] <- data1[ , "year"] - unlist(data1[ , "month"]) %in% abs(month_adj)
      }
    }
    
  } # end ~ Create data set for analysis
  
  # ----< Process data at annual level >----
  {
    # Average data by year ####
    ffyr <- data1 %>%
      group_by(., year) %>%
      summarise(., avg = mean(.data[[value_col]]))
    
    # Compute annual quantiles ####
    quan_val <- quantile(ffyr$avg, probabilities)
    quan_val <- quan_val + c(-0.1, rep(0,length(quan_val)-2), 0.1) # extend end points slightly
    
    # Add quantile class to annual data set ####
    ffyr <- ffyr %>%
      mutate(., year_cat = cut(avg, quan_val, labels = FALSE))
  } # end ~ Process data at annual level
  
  # ----< Process data at monthly level >----
  {
    # Average data by year+month ####
    ffmo <- data1 %>%
      group_by(., year, month) %>%
      summarise(., avg = mean(.data[[value_col]]), .groups = "keep") %>%
      ungroup(.)

    # process each month ####  
    for(mon in month_order) {
      
      # extract one month
      ff1mo <- ffmo %>%
        filter(., month == mon)
      
      # compute monthly quantiles
      quan_val <- quantile(ff1mo$avg, probabilities)
      quan_val <- quan_val + c(-0.1, rep(0,length(quan_val)-2), 0.1) # extend end points slightly
      
      # Add quantile class to monthly data set
      ff1mo <- ff1mo %>%
        mutate(.
          , !!month.abb[mon] := cut(ff1mo$avg, quan_val, labels = FALSE)) %>%
        select(., year, month.abb[mon])
  
      # Merge one month data results to overall data set    
      ffyr <- ffyr %>%
        left_join(., ff1mo, by = "year")
      
    } # end ~ for mon
  } # end ~ Process data at monthly level

  # ----< Filter final data set down to selected years >----
  {
  ffyr <- ffyr %>%
    filter(., year >= start_year
      , year <= end_year) %>%
    rename(., year_avg = avg)
  } # end ~ Filter final data set down
  
  FT <- tblFT1(ffyr
    , tbl_title = "Yearly and monthly quantile classes"  
  )
  
  return(ffyr)
  
} # end ~ function: calcQuanClass
  