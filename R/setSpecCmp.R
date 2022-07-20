# ####
#' @title Complete c.spec with supporting variables
#' 
#' @description Complete c.spec with supporting variables
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom tibble tibble as_tibble 
#' @importFrom scales col_numeric
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecCmp <- function(c.spec) {
  
  # ----< extract needed variables >----
  varsNeeded <- c("statVec", "startYear", "endYear", "monthGrid", "dayGrid"
    , "grpCnt", "wqParm", "wqLayer", "idVar", "profVar", "monthAdj"
    , "analysisTitle", "analysisDate", "filename", "dataOut", "exCovClass")
  extract(c.spec, varsNeeded)
  
  # ----< Station setup: labels and order based on statVec >----
  statDF <- tibble(ord = 1:length(statVec)
    , statVec
    , lab = statVec)
  
  # ----< Year setup: labels and order based on startYear and endYear >----
  yearVec = startYear:endYear
  yearDF <- tibble(ord = 1:length(yearVec)
    , yearVec
    , lab =  paste(yearVec))
  
  # ----< Month setup: vector, labels and order based on monthGrid and monthAdj >----
  monthVec <- monthGrid
  if (exists("monthAdj") && !(any(is.na(monthAdj)) | is.null(monthAdj))) {
    if (monthAdj[1] > 0) {
      monthOrd <- c(monthVec[monthVec %in% monthAdj], monthVec[!(monthVec %in% monthAdj)])
    } else {
      monthOrd <- c(monthVec[!(monthVec %in% abs(monthAdj))], monthVec[(monthVec %in% abs(monthAdj))])
    }
  } else {
    monthOrd <- NA
  }
  monthDF <- tibble(ord = 1:length(monthOrd)
    , monthVec = monthOrd
    , lab = month.abb[monthOrd])
  
  # ----< append variables to list for return >----
  vars2append <- c("statDF", "yearDF", "monthDF")
  
  for (var in vars2append) {
    c.spec[[var]] <- eval(parse(text=var))
  }  
  
  # ----< datSource specific supplemental variables >----
  if(c.spec$datSource == "gam") {
    c.spec <- setSpecCmpGAM(c.spec) 
  } else if (c.spec$datSource == "WRTDS") {
    c.spec <- setSpecCmpWRTDS(c.spec) 
  }
  
   
  return(c.spec)
  
} # end ~ function: setSpecCmp


