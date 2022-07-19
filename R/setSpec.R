# ####
#' @title Create (or update) specification list for cluster analysis
#' 
#' @description Organize user-provided cluster analysis specifications into a
#'   list. And where specifications are not provided then make some default
#'   selections.
#'   
#' @details ...
#' potVar projRoot root folder for analysis
#' potVar gamRoot folder where output from baytrends is stored. There should be
#'   subfolders in this folder with names corresponding to the various parameters
#'   of interest. For example, if \code{wqParm} is set to \code{"chla"}, then there should 
#'   be a subdirectory under the \code{gamRoot} folder called \code{chla}. 
#' potVar wqParm parameter to be analyzed
#' potVar waterBody waterBody
#' potVar slayer slayer
#' potVar gamNumbr gamNumbr
#' potVar startYear First year of data to be used in cluster analysis
#' potVar endYear Last year of data to be used in cluster analysis
#' potVar idVar Variable that identifies the rows of the cluster matrix, i.e. items that are clustered 
#' potVar profVar Variable that becomes columns of the cluster matrix, i.e. attribute to cluster
#' potVar rowAdj rowAdj
#' potVar scale  scale
#' potVar grpCnt grpCnt
#' potVar grpCol grpCol
#' potVar statVec statVec
#' potVar statLab statLab
#' potVar monVec monVec
#' potVar monLab monLab
#' potVar filename filename=NA
#' potVar monOrd monOrd=NA,
#' potVar yrOrd yrOrd=NA,
#' potVar dataOut dataOut=NA,
#' potVar subGrps subGrps=FALSE,
#' potVar addFlow addFlow,
#' potVar flowFile flowFile,
#' potVar flowLab flowLab='Flow',
#' potVar datSource datSource
#' potVar datFile datFile,
#' potVar lineOne lineOne,
#' potVar grpLab grpLab=NA,
#' potVar yearType yearType = 'calendar'
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

  # find common variable names between arguments passed to those in original c.spec
  varCommon <- intersect(names(c.spec2), names(c.spec))
  
  # down-select common variables to those with updates
  varCommonDifferent <- varCommon[(length(unlist(c.spec[varCommon])) != length(unlist(c.spec2[varCommon]))) ||
      unlist(c.spec[varCommon]) != unlist(c.spec2[varCommon]) ]

  # find new variables passed as argument 
  varNew <- setdiff(names(c.spec2), names(c.spec))
  
  # update c.spec based on changed variables and new variables
  c.spec[c(varCommonDifferent, varNew)] <- c.spec2[c(varCommonDifferent, varNew)]
  
  # create table of changes
  df <- tibble(Variable = c(varCommonDifferent, varNew)) %>%
    left_join(., c.specQC, by = "Variable") %>%
    select(., Variable, Description) %>%
    mutate(., Settings = NA_character_)

  for (k1 in 1:NROW(df)) {
    df[k1, "Settings"] <- vec.strg(as.character(unlist(c.spec[[unlist(df[k1, "Variable"])]])))
  }
  
  # print the updates out
  FT <- tblFT1(df)

  return(c.spec)
}
