# ####
#' @title Output formatted table. 
#' 
#' @description This function outputs a table of data with basic formatting.
#'   
#' @details ...
#' 
#' 
#' @param data table data
#' @param tblTitle table title
#' @param tblPre_label table title pre_label
#' @param tblFontName table font
#' @param tblFontSize font size
#' @param tblTheme table theme
#' @param tblOutput TRUE to output table
#' @param tblReturn TRUE to return formatted table 
#' @param tbltoRMD TRUE to apply flextable_to_rmd(...)
#' 
#' @examples 
#' \dontrun{
#' 
#' tblFT1(head(iris))
#' 
#' }
#' 
#' @return 
#' # may return flextable 
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by rename_with
#' @importFrom flextable flextable align fontsize font padding  set_caption flextable_to_rmd
#' @importFrom flextable theme_box theme_vanilla theme_booktabs
#' @importFrom officer run_autonum
#' 
#' @export
#' 
tblFT1 <- function(data
  , tblTitle = NA
  , tblPre_label = NA   # "Table: "
  , tblFontName = NA    # "Calibri"
  , tblFontSize = NA    # 11
  , tblTheme = NA       # "box"
  , tblOutput = TRUE
  , tblReturn = FALSE
  , tbltoRMD = FALSE
  ) {
  
  # ----< create flextable >----
  FT <- flextable(data) %>%
    align(align = "right", part = "all") %>%
    padding(padding = 1, part = "all") 
  
  # ----< customize font name >----
  if (!is.na(tblFontName)) {
    FT <- font(FT, fontname = tblFontName, part = "all")
  }  

  # ----< customize font size >----  
  if (!is.na(tblFontSize)) {
    FT <- fontsize(FT, size = tblFontSize, part = "all")
  }
  
  # ----< customize table theme >----
  stopifnot(tblTheme %in% c(NA, "box", "vanilla", "booktabs"))
  if (!is.na(tblTheme)) {
    if (tblTheme == "box") {
      FT <- theme_box(FT)   
    } else if (tblTheme == "vanilla") {
      FT <- theme_vanilla(FT)   
    } else if (tblTheme == "booktabs") {
      FT <- theme_booktabs(FT)   
    } 
  }
  
  # ----< customize table title >----
  if (!is.na(tblTitle)) {
    FT <- set_caption(FT, caption = tblTitle
      , autonum = officer::run_autonum(seq_id = "tab", pre_label = tblPre_label, bkm = "anytable"))
  }
  
  # ----< customize output >----
  if (tblOutput) {
    if (tbltoRMD) {
      flextable_to_rmd(FT)  
    } else {
      FT
    }
  }
  
  if (tblReturn) return(FT)
  
}
