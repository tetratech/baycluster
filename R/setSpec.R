# ####
#' @title Create (or update) specification list for cluster analysis
#' 
#' @description Organize user-provided cluster analysis specifications into a
#'   list. And where specifications are not provided then make some default
#'   selections.
#'   
#' @details Below is the list of arguments needed to run a cluster analysis:
#' \itemize{
#' \item analysisTitle - Analysis title 
#' \item analysisDate - Analysis date 
#' \item datSource - Type of analysis 
#' \item projFolder - Project folder 
#' \item gamFolder - GAM folder 
#' \item datFolder - Input data folder 
#' \item outFolder - Output data folder 
#' \item statFile - Station file name 
#' \item startYear - Start year 
#' \item endYear - End year 
#' \item monthGrid - Months included 
#' \item dayGrid - Days of month 
#' \item monthAdj - Month adjustment 
#' \item statVec - Station list 
#' \item wqParm - Water quality parameter 
#' \item wqLayer - Layer 
#' \item gamNumbr - GAM #'s 
#' \item idVar - Items that are to be clustered
#' \item profVar - Attribute to cluster by 
#' \item grpCnt - Number of cluster 
#' \item dataTrans - Data transformation 
#' \item dataCenter - Data centering 
#' \item monthGracePeriod - Grace period 
#' \item exCovIncl - Include exogenous covariate 
#' \item exCovFolder - Ex. Cov. Folder 
#' \item exCovLab - Ex. Cov. Label 
#' \item exCovFile - Ex. Cov. File 
#' \item exCovClass - Ex. Cov. # classes 
#' \item exCovTrans - Ex. Cov. Transformation 
#' }
#' 
#' @param c.spec list for storing cluster analysis specifications
#' @param ... specifications needed to run cluster analysis -- see details
#' 
#' @examples 
#' \dontrun{
#' #TBD
#' 
#' }
#' 
#' @return list
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble 
#' 
#' @export
#'
setSpec <- function(c.spec = list(), ...) {
  
  c.spec2 <- grabFunctionParameters()   # create list of function arguments
  c.spec2$c.spec <- NULL                # drop c.spec from list

  # find common variable names between arguments passed to those in original c.spec ####
  varCommon <- intersect(names(c.spec2), names(c.spec))
  
  # down-select common variables to those with updates ####
  chk <- logical(length = length(varCommon))
  for (k1 in 1:length(varCommon)) {
    var = varCommon[k1]
    chk[k1] = length(unlist(c.spec[var])) != length(unlist(c.spec2[var])) ||
      unlist(c.spec[var]) != unlist(c.spec2[var])
  }
  varCommonDifferent <- varCommon[chk]
  
  # find new variables passed as argument  ####
  varNew <- setdiff(names(c.spec2), names(c.spec))
  
  # update c.spec based on changed variables and new variables ####
  c.spec[c(varCommonDifferent, varNew)] <- c.spec2[c(varCommonDifferent, varNew)]
  
  # create table of changes ####
  df <- tibble(Variable = c(varCommonDifferent, varNew)) %>%
    left_join(., c.specQC, by = "Variable") %>%
    select(., Variable, Description) %>%
    mutate(., Settings = NA_character_)

  for (k1 in 1:NROW(df)) {
    df[k1, "Settings"] <- vec.strg(as.character(unlist(c.spec[[unlist(df[k1, "Variable"])]])))
  }
  
  # print the updates out ####
  FT <- tblFT1(df)
  
  # do rudimentary checking ####
  setSpecCheck(c.spec)
  
  # build out c.spec ####
  c.spec <- setSpecBuildout(c.spec)

  return(c.spec)
}
