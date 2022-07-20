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
#' @importFrom dplyr %>% mutate select filter 
#' @importFrom flextable flextable align fontsize font padding  set_caption flextable_to_rmd
#' @importFrom flextable theme_box theme_vanilla theme_booktabs
#' @importFrom officer run_autonum
#' @importFrom rstudioapi getActiveDocumentContext isAvailable
#' 
#' @export
#' 
tblFT1 <- function(data
  , tblTitle = NA
  , tblPre_label = NA   # "Table: "
  , tblFontName = NA    # "Calibri"
  , tblFontSize = NA    # 11
  , tblTheme = NA       # "box" 
  ) {
  
  # ----< Interactive >----
  isInteractive <- isAvailable()
  ctxt <- getActiveDocumentContext()
  isRmd <- ifelse(grepl("\\.Rmd$", ctxt$path), TRUE, FALSE)
  
  # ----< create flextable >----
  FT <- flextable(data) %>%
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
      , autonum = run_autonum(seq_id = "tab", pre_label = tblPre_label, bkm = "anytable"))
  }
  
  # # ----< customize output >----
  if (isInteractive) {
    print(FT, preview = "html")
  } else {
    flextable_to_rmd(FT)  
  }
  
  return(FT)

}
