# ####
#' @title Output formatted table. 
#' 
#' @description This function outputs a table of data with basic formatting.
#'   
#' @details ...
#' 
#' 
#' @param data data set to output
#' @param tbl_title table title
#' @param tbl_pre_label table title pre_label, e.g., "Table: "
#' @param tbl_font_name table font, e.g., "Calibri"
#' @param tbl_font_size font size
#' @param tbl_theme table theme, available options: "box", "vanilla", "booktabs"
#' 
#' @examples 
#' \dontrun{
#' tblFT1(head(iris))
#' 
#' tblFT1(head(iris)
#'   , tbl_theme ="box")
#' }
#' 
#' @return 
#' # may return flextable 
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom dplyr %>% mutate select filter 
#' @importFrom flextable flextable align fontsize font padding  set_caption
#'   flextable_to_rmd theme_box theme_vanilla theme_booktabs colformat_int
#'   colformat_double
#' @importFrom officer run_autonum
#' @importFrom rstudioapi getActiveDocumentContext isAvailable
#' 
#' @export
#' 
tblFT1 <- function(data
  , tbl_title = NA
  , tbl_pre_label = NA   # "Table: "
  , tbl_font_name = NA    # "Calibri"
  , tbl_font_size = NA    # 11
  , tbl_theme = NA       # "box" 
  ) {
  
  # ----< Interactive >----
  isInteractive <- isAvailable()
  ctxt <- getActiveDocumentContext()
  isRmd <- ifelse(grepl("\\.Rmd$", ctxt$path), TRUE, FALSE)
  
  # ----< create flextable >----
  FT <- flextable(data) %>%
    padding(padding = 1, part = "all") 
  
  # %>%
  #   colformat_int(big.mark = "") %>%
  #   colformat_double(big.mark = "")
  
  # ----< customize font name >----
  if (!is.na(tbl_font_name)) {
    FT <- font(FT, fontname = tbl_font_name, part = "all")
  }  

  # ----< customize font size >----  
  if (!is.na(tbl_font_size)) {
    FT <- fontsize(FT, size = tbl_font_size, part = "all")
  }
  
  # ----< customize table theme >----
  stopifnot(tbl_theme %in% c(NA, "box", "vanilla", "booktabs"))
  if (!is.na(tbl_theme)) {
    if (tbl_theme == "box") {
      FT <- theme_box(FT)   
    } else if (tbl_theme == "vanilla") {
      FT <- theme_vanilla(FT)   
    } else if (tbl_theme == "booktabs") {
      FT <- theme_booktabs(FT)   
    } 
  }
  
  # ----< customize table title >----
  if (!is.na(tbl_title)) {
    FT <- set_caption(FT, caption = tbl_title
      , autonum = run_autonum(seq_id = "tab", pre_label = tbl_pre_label, bkm = "anytable"))
  }
  
  # # ----< customize output >----
  if (isInteractive) {
    print(FT, preview = "html")
  } else {
    flextable_to_rmd(FT)  
  }
  
  return(FT)

}
