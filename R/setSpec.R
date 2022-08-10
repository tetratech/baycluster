# ####
#' @title Create (or update) specification list for cluster analysis 
#' 
#' @description Create (or update) specification list for cluster analysis.
#'   Currently developed for using predictions from GAM results. (Future updates
#'   for WRTDS files.)
#'   
#' @details See \code{\link{c.spec_qc}} for list of variables needed to be passed. 
#' 
#' TBD: expand to include list of variables added to the list
#' 
#' @param c.spec list for storing specifications for cluster analysis 
#' @param ... specifications needed to run cluster analysis -- see details for
#'   list of needed variables
#' 
#' @examples 
#' \dontrun{
#' c.spec <- setSpec(c.spec = list()
#'   , start_year    = 1993
#'   , end_year      = 2020
#'   , month_grid    = 1:12
#'   , day_grid      = 15 
#'   , month_adj     = NA
#'   ) 
#' 
#' }
#' 
#' @return list
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by all_of desc count last_col
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble 
#' 
#' @export
#'
setSpec <- function(c.spec = list(), ...) {
  
  # ----< Create list of arguments passed in function >----
  {
    c.spec2 <- grabFunctionArguments()    
    c.spec2$c.spec <- NULL                # drop c.spec from list
  } 
  
  # ----< Find updates of existing variables >----
  {
    # find common variable names between arguments passed to those in original c.spec ####
    varCommon <- intersect(names(c.spec2), names(c.spec)) 
    
    # which common variables are different ####
    chk <- rep(TRUE, length(varCommon)) 
    
    for (k1 in 1:length(varCommon)) { 
      var <- varCommon[k1]
      chk[k1] <- !identical( c.spec[var], c.spec2[var])
    } 
    
    # Down-select to variables with updates needed ####    
    varCommonDifferent <- varCommon[chk]
    
  }
  
  # ----< Find new variables >---- 
  { 
    varNew <- setdiff(names(c.spec2), names(c.spec)) 
  }
  
  # ----< Update c.spec >----
  {
    # updates based on changed variables and new variables 
    if (length(c(varCommonDifferent, varNew)) > 0) {
      c.spec[c(varCommonDifferent, varNew)] <- c.spec2[c(varCommonDifferent, varNew)]
    }
  }
  
  # ----< Create table of changes >---- 
  {
    # create table of variables with updates
    df <- tibble(Variable = c(varCommonDifferent, varNew)) %>%
      left_join(., c.spec_qc, by = "Variable") %>%
      select(., Variable, Description) %>%
      mutate(., Settings = NA_character_)
    
    # add changes
    if (NROW(df) == 0) {
      df[1,"Variable"] <- "No changes detected."
    } else {
      for (k1 in 1:NROW(df)) {
        df[k1, "Settings"] <- vec.strg(as.character(unlist(c.spec[[unlist(df[k1, "Variable"])]])))
      }
    }
    
    # print the updates out ####
    FT <- tblFT1(df
      , tbl_title = "Cluster analysis settings/updates"  
    )
  }
  
  # ----< Chk (Check) c.spec >----
  {
    setSpecChk(c.spec)
  }
  
  # ----< Cmp (Complete) c.spec >----
  {
    c.spec <- setSpecCmp(c.spec)
  }
  
  return(c.spec)
  
} # end ~ function: setSpec
