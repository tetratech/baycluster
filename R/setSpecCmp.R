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
  pry(c.spec, varsNeeded)

  # ----< Station setup: labels and order based on statVec >----
  print(statVec)
  statDF <- tibble(statOrd = 1:length(statVec)
    , statVec
    , statLab = statVec)
  print(statDF)
  
  if ("stations" %in% names(c.spec)) {
    stations <- c.spec$stations
    varFound <- grep("station", names(stations), ignore.case = TRUE , value = TRUE)
    statDF <- merge(statDF, stations, by.x = "statVec", by.y = varFound, all.x = TRUE)
  }
  print(statDF)
  
  statDF <- statDF %>%
    arrange(., statOrd)
  print(statDF)
  
  # ----< Year setup: labels and order based on startYear and endYear >----
  yearVec = startYear:endYear
  yearDF <- tibble(yearOrd = 1:length(yearVec)
    , yearVec
    , yearLab =  paste(yearVec))
  
  # ----< Month setup: vector, labels and order based on monthGrid and monthAdj >----
  monthOrd <- monthVec <- monthGrid
  if (any(!is.na(monthAdj))) {
    if (monthAdj[1] > 0) {
      monthOrd <- c(monthVec[monthVec %in% monthAdj], monthVec[!(monthVec %in% monthAdj)])
    } else {
      monthOrd <- c(monthVec[!(monthVec %in% abs(monthAdj))], monthVec[(monthVec %in% abs(monthAdj))])
    }
  }
  monthDF <- tibble(monthOrd.1 = 1:length(monthOrd)
    , monthVec = monthOrd
    , monthLab = month.abb[monthOrd]) %>%
    rename(., monthOrd = monthOrd.1)
  
  # ----< creating these lists makes it easy to switch between clustering by years, months, or stations. >----
  idLev <- list(
    year = yearDF$yearVec
    , month = monthDF$monthVec
    , station = statDF$statVec)
  
  idLab <- list(
    year = yearDF$yearLab
    , month = monthDF$monthLab
    , station = statDF$statLab)
  
  # ----< append variables to list for return >----
  vars2append <- c("statDF", "yearDF", "monthDF", "idLev", "idLab")
  
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


