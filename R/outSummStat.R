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
#' 
#' @examples 
#' # TBD
#' 
#' @return 
#' # returns flextable list
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by rename_with
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tidyr pivot_wider pivot_longer 
#' @importFrom tibble tibble as_tibble rownames_to_column is_tibble
#' @importFrom readr read_lines read_delim
#' @importFrom flextable flextable align fontsize font padding theme_box set_caption
#' @importFrom officer run_autonum
#' 
#' @export
#' 
outSummStat <- function(data) {
  
  boo.numeric = TRUE
  boo.character = TRUE
  boo.date = TRUE
  FT1 <- FT2 <- FT3 <- NA
  
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
      
      FT2 <- data %>% 
        summarise(across( where(is.numeric), ~ quibble(.x, c(0, 0.25, 0.50, 0.75, 1.0), 
          dropNA = TRUE)), .groups = 'drop') %>% 
        tidyr::unnest(where(is_tibble), names_repair = 'unique', names_sep = "_") %>%
        select(., -ends_with("_q")) %>%
        rename_with(.,  ~ gsub("_x", "", .x, fixed = TRUE)) %>%
        rownames_to_column() %>%
        pivot_longer(., cols = where(is.numeric)) %>%
        pivot_wider(., names_from = rowname) %>%
        rename(., Variable=name, Min=`1`, Q25=`2`, Med=`3`, Q75=`4`, Max=`5`) 
      
      FT2 <- flextable::flextable(FT2) %>%
        flextable::align(align = "right", part = "all") %>%
        flextable::fontsize(size=11, part = "all") %>%
        flextable::font(fontname = "Calibri", part = "all") %>%
        flextable::padding(padding = 1, part = "all") %>%
        flextable::theme_box() %>%
        flextable::set_caption("Summary statistics-numeric fields"
          , autonum = officer::run_autonum(seq_id = "tab", pre_label = "Table: ", bkm = "anytable"))
    }    
  }
  
  # ----< date fields >----
  {
    
    if (sum(i3) > 0) {
      
      x <- data.frame(unclass(summary(data[,i3])), check.names = FALSE, stringsAsFactors = FALSE)
      x <- data.frame(x[c(1,2,3,5,6), ])
      rownames(x) <- NULL
      names(x) <- names(data)[i3]
      x <- as.data.frame(do.call(cbind, lapply(x, substring, first=9)))
      x <- suppressMessages(as_tibble(cbind(nms = names(x), t(x)), .name_repair = 'unique'))
      names(x) <- c("Variable", "Min", "Q25", "Med", "Q75", "Max")
      x
      
      FT3 <- flextable::flextable(x) %>%
        flextable::align(align = "right", part = "all") %>%
        flextable::fontsize(size=11, part = "all") %>%
        flextable::font(fontname = "Calibri", part = "all") %>%
        flextable::padding(padding = 1, part = "all") %>%
        flextable::theme_box() %>% 
        flextable::set_caption("Summary statistics-date fields"
          , autonum = officer::run_autonum(seq_id = "tab", pre_label = "Table: ", bkm = "anytable"))
      
    } 
  }
  
  # ----< character fields >----
  {
    # need to do something here.
  }
  
  if (boo.numeric & sum(i2) > 0) print(FT2)
  if (boo.date & sum(i3) > 0) print(FT3)
  
  
  # return stuff
  FT.return <- list(char = FT1, num = FT2, date = FT3 ) 
  return(FT.return)
  
}