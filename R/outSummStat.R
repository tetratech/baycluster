# ####
#' @title Output summary statistics about table into table
#' 
#' @description This function outputs the 5-number summary for numeric and
#' date fields. Character arrays are summarized.
#'   
#' @details ...
#' 
#' 
#' @param data table variable
#' @param tblFTout set to TRUE to output formatted tables
#' 
#' @examples 
#' \dontrun{
#' 
#' outSummStat(iris)
#' 
#' }
#' 
#' @return 
#' # returns flextable list
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data
#' @importFrom lubridate ymd decimal_date yday year month is.Date
#' @importFrom lubridate %m+% %m-% make_date  floor_date ceiling_date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename 
#' @importFrom dplyr group_by rename_with distinct relocate left_join arrange
#' @importFrom dplyr  between pull summarise ungroup across ends_with   
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tibble tibble as_tibble rownames_to_column is_tibble
#' @importFrom readr read_lines read_delim
#' @importFrom flextable flextable align fontsize font padding theme_box set_caption flextable_to_rmd
#' @importFrom officer run_autonum
#' @importFrom stats quantile
#' 
#' @export
#' 
outSummStat <- function(data, tblFTout = FALSE) {
  
  # initialize return variables
  x.numSum <- x.dateSum <- x.charSum <- NA
  
  # initialize formatted output options
  if (tblFTout) {
    outtblFT.numeric <- outtblFT.character <- outtblFT.date <- TRUE
  } else {
    outtblFT.numeric <- outtblFT.character <- outtblFT.date <- FALSE  
  }
  
  # determine character, numeric and date fields
  i1 <- sapply(data, is.character) 
  i2 <- sapply(data, is.numeric) 
  i3 <- sapply(data, lubridate::is.Date) 
  
  # ----< numeric fields >----
  {
    if (sum(i2) > 0) {
      
      # summary statistics function
      quibble <- function(x, q = c(0, 0.25, 0.50, 0.75, 1.0), dropNA = TRUE, type=1) {
        tibble(x = quantile(x, q, na.rm = dropNA), q = q)
      }
      
      # apply summary statistic function
      x.numSum <- data %>% 
        summarise(across( where(is.numeric), ~ quibble(.x, c(0, 0.25, 0.50, 0.75, 1.0), 
          dropNA = TRUE)), .groups = 'drop') %>% 
        tidyr::unnest(where(is_tibble), names_repair = 'unique', names_sep = "_") %>%
        select(., -ends_with("_q")) %>%
        rename_with(.,  ~ gsub("_x", "", .x, fixed = TRUE)) %>%
        rownames_to_column() %>%
        pivot_longer(., cols = where(is.numeric)) %>%
        pivot_wider(., names_from = rowname) %>%
        rename(., Variable=name, Min=`1`, Q25=`2`, Med=`3`, Q75=`4`, Max=`5`) 
      
      # output formatted table    
      if (outtblFT.numeric)  {
        tblFT1(x.num
          , tblTitle = "Summary statistics-numeric fields"
          , tblPre_label = "Table: "
          , tblFontName = "Calibri"
          , tblFontSize = 11
          , tblTheme = "box"
          , tblOutput = TRUE
          , tblReturn = FALSE
          , tbltoRMD = TRUE)
      }
    }
  } # end ~ numeric
  
  # ----< date fields >----
  {
    if (sum(i3) > 0) {
      
      # create a 5-number summary for date fields      
      x <- data.frame(unclass(summary(data[,i3])), check.names = FALSE, stringsAsFactors = FALSE)
      x <- data.frame(x[c(1,2,3,5,6), ])
      rownames(x) <- NULL
      names(x) <- names(data)[i3]
      x <- as.data.frame(do.call(cbind, lapply(x, substring, first=9)))
      x.dateSum <- suppressMessages(as_tibble(cbind(nms = names(x), t(x)), .name_repair = 'unique'))
      names(x.dateSum) <- c("Variable", "Min", "Q25", "Med", "Q75", "Max")
      
      # output formatted table
      if (outtblFT.date) {
        tblFT1(x.dateSum
          , tblTitle = "Summary statistics-date fields"
          , tblPre_label = "Table: "
          , tblFontName = "Calibri"
          , tblFontSize = 11
          , tblTheme = "box"
          , tblOutput = TRUE
          , tblReturn = FALSE
          , tbltoRMD = TRUE)
      }
    }
  } # end ~ date fields

  # ----< character fields >----
  {
    if (sum(i3) > 0) {
     
      x.charSum <- header(data[ , i3])
      
      if (outtblFT.character) {
        tblFT1(x.charSum
          , tblTitle = "Header-character fields"
          , tblPre_label = "Table: "
          , tblFontName = "Calibri"
          , tblFontSize = 11
          , tblTheme = "box"
          , tblOutput = TRUE
          , tblReturn = FALSE)
      }
    }
  }

    
  # return summary statistics
  FT.return <- list(char = x.charSum, num = x.numSum, date = x.dateSum ) 
  return(FT.return)
  
} # end ~ function: outSummStat




